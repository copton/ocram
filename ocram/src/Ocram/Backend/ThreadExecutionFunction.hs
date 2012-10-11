{-# LANGUAGE TemplateHaskell, GADTs#-}
module Ocram.Backend.ThreadExecutionFunction
-- exports {{{1
(
  thread_execution_functions
) where

-- imports {{{1
import Compiler.Hoopl (postorder_dfs_from, foldGraphNodes, successors, entryLabel, O, C, mapLookup)
import Control.Arrow (second)
import Data.Generics (everywhere, mkT)
import Data.Maybe (mapMaybe, maybeToList)
import Language.C.Data.Ident (Ident, internalIdent)
import Language.C.Syntax.AST
import Ocram.Analysis (CallGraph, call_order, start_functions, call_chain, is_blocking)
import Ocram.Query (function_parameters_fd, function_parameters_cd)
import Ocram.Intermediate
import Ocram.Debug (CExpr', VarMap', eun, CBlockItem', set_thread, CFunDef', ENodeInfo(..), aset, set_blocking)
import Ocram.Print (render)
import Ocram.Ruab (Variable(AutomaticVariable), FQN)
import Ocram.Names (mangleFun, contLbl, tfunction, contVar, frameUnion, tstackVar, resVar, estackVar)
import Ocram.Symbols (Symbol, symbol)
import Ocram.Util ((?:), fromJust_s, abort, lookup_s)
import Prelude hiding (last)

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Compiler.Hoopl as H

thread_execution_functions :: CallGraph -> M.Map Symbol CDecl -> M.Map Symbol Function -> M.Map Symbol (Maybe CDecl) -> ([CFunDef'], VarMap') -- {{{1
thread_execution_functions cg bf cf estacks = second concat $ unzip $ zipWith tef [0..] (start_functions cg)
  where
    tef tid sf = threadExecutionFunction cg bf cf ($lookup_s estacks sf) tid sf

threadExecutionFunction :: CallGraph -> M.Map Symbol CDecl -> M.Map Symbol Function -> Maybe CDecl -> Int -> Symbol -> (CFunDef', VarMap') -- {{{2
threadExecutionFunction cg bf cf estack tid name = (fmap (set_thread tid) fun, concat vms)
  where
    tsf = $lookup_s cf name
    estack' = fmap (aset EnUndefined) estack

    fun =
      CFunDef [CTypeSpec (CVoidType eun)] (CDeclr (Just (ii (tfunction tid))) [fdeclr] Nothing [] eun) [] body eun

    fdeclr =
      CFunDeclr (Right ([CDecl [CTypeSpec (CVoidType eun)] [(Just (CDeclr (Just (ii contVar)) [CPtrDeclr [] eun] Nothing [] eun), Nothing, Nothing)] eun], False)) [] eun

    body =
      CCompound [] (fmap CBlockDecl estack' ?: intro : concat functions) eun

    intro =
      CBlockStmt (CIf (CVar (ii contVar) eun) (CGotoPtr (CVar (ii contVar) eun) eun) Nothing eun)

    (functions, vms) =  unzip . map (inlineCriticalFunction cg tid callees entries name) . mapMaybe (flip M.lookup cf) . $fromJust_s . call_order cg $ name

    callees = M.map function_parameters_cd bf `M.union` M.map (function_parameters_fd . fun_def) cf
    entries = M.map fun_entry cf

inlineCriticalFunction :: CallGraph -> Int -> M.Map Symbol [CDecl] -> M.Map Symbol Label -> Symbol -> Function -> ([CBlockItem'], VarMap') -- {{{2
inlineCriticalFunction cg tid callees entries startFunction inlinedFunction = (cGraph blocks, vm)
  where
    fname     = fun_name inlinedFunction
    callChain = $fromJust_s $ call_chain cg startFunction fname
    suls      = singleUsageLabels (fun_entry inlinedFunction) (fun_body inlinedFunction)
    blocks    = block_map $ fun_body inlinedFunction

    vm = map vmEntry $ filter isTVar $ (fun_cVars inlinedFunction ++ fun_ncVars inlinedFunction)
    vmEntry var = (AutomaticVariable tid (var_tname var), var_scope var, symbol2fqn (var_tname var))

    isTVar (TVariable _ _ _) = True
    isTVar _                 = False

    cGraph :: BlockMap -> [CBlockItem']
    cGraph lm =
      let entry = Goto (fun_entry inlinedFunction) in
        concatMap cBlock
      $ filter (not . flip S.member suls . entryLabel)
      $ postorder_dfs_from lm entry

    cBlock :: Block -> [CBlockItem']
    cBlock block =
      let (first, middles, last) = block_components block in
        cFirst first ++ map cMiddle middles ++ cLast last

    inlineBlock :: Label -> [CBlockItem']
    inlineBlock lbl = 
      let
        block = $fromJust_s $ mapLookup (hLabel lbl) blocks
        (_, middles, last) = block_components block
      in
        map cMiddle middles ++ cLast last

    cFirst :: Node C O -> [CBlockItem']
    cFirst (Label entry) = [CBlockStmt (CLabel (lblIdent entry fname) (CExpr Nothing eun) [] eun)]

    cFirst (Cont lbl (FirstNormalForm _ _ _)) = cFirst (Label lbl)
    cFirst (Cont lbl (SecondNormalForm lhs op callee _ eni)) =
      let 
        callChain' = callChain ++ [callee] 
        lhs'       = rewriteLocalVariableAccess lhs
        resultExpr = CExpr (Just (CAssign op lhs' (tstackAccess callChain' (Just resVar) eni) eni)) eni
      in cFirst (Label lbl) ++ [CBlockStmt resultExpr]

    cMiddle :: Node O O -> CBlockItem'
    cMiddle (Stmt expr) = CBlockStmt $ CExpr (Just (cExpr expr)) (annotation expr)
    
    cLast :: Node O C -> [CBlockItem']

    cLast (Return mexpr eni)
      | startFunction == fname = [CBlockStmt (CReturn (fmap cExpr mexpr) eni)]
      | otherwise              = map CBlockStmt $ 
           maybeToList (fmap assignResult mexpr)
        ++ [CGotoPtr (tstackAccess callChain (Just contVar) eni) eni]
      where assignResult expr = CExpr (Just (CAssign CAssignOp (tstackAccess callChain (Just resVar) eni) (cExpr expr) eni)) eni

    cLast (Goto lbl)
      | S.member (hLabel lbl) suls = inlineBlock lbl
      | otherwise                  = [CBlockStmt $ CGoto (lblIdent lbl fname) eun] -- XXX missing debug info

    cLast (If cond t e eni) = [CBlockStmt $ CIf (cExpr cond) (cBranch eni t) (Just (cBranch eni e)) eni]

    cLast (Call (FirstNormalForm callee params eni) lbl) =
         criticalCallSequence callee (map cExpr params) eni lbl

    cLast (Call (SecondNormalForm  _ _ callee params eni) lbl) =
         criticalCallSequence callee (map cExpr params) eni lbl

    cBranch eni lbl
      | S.member (hLabel lbl) suls = CCompound [] (inlineBlock lbl) eun
      | otherwise = CCompound [] [CBlockStmt (CGoto (lblIdent lbl fname) eni)] eun

    cExpr :: CExpr' -> CExpr'
    cExpr = everywhere (mkT rewriteLocalVariableAccess)

    criticalCallSequence callee params eni lbl = -- {{{3
      map CBlockStmt $ parameters ++ continuation : callExp : returnExp ?: []
      where
        callChain' = callChain ++ [callee]
        blocking   = is_blocking cg callee

        parameters = zipWith paramAssign params ($lookup_s callees callee)

        continuation = assign (tstackAccess callChain' (Just contVar) eni) (CLabAddrExpr (lblIdent lbl fname) eni)

        callExp
          | blocking  = CExpr (Just (CCall (CVar (ii callee) eun) [CUnary CAdrOp (tstackAccess callChain' Nothing eun) eun] eun)) (set_blocking eni)
          | otherwise = CGoto (lblIdent ($lookup_s entries callee) callee) eni

        returnExp
          | blocking  = Just $ CReturn Nothing eni
          | otherwise = Nothing

        paramAssign e decl = assign (tstackAccess callChain' (Just (symbol decl)) eni) e 
        assign lhs rhs = CExpr (Just (CAssign CAssignOp lhs rhs eni)) eni

    rewriteLocalVariableAccess :: CExpr' -> CExpr' -- {{{3
    rewriteLocalVariableAccess o@(CVar iden eni)
      | test fun_cVars  = tstackAccess callChain (Just vname) eni
      | test fun_ncVars = estackAccess (fun_name inlinedFunction) vname eni
      | test fun_stVars = staticAccess vname eni
      | otherwise       = o
      where
        vname = symbol iden
        test f = vname `elem` map var_unique (f inlinedFunction)
    rewriteLocalVariableAccess o = o

    symbol2fqn :: Symbol -> FQN -- {{{3
    symbol2fqn name =
      render $ rewriteLocalVariableAccess (CVar (internalIdent name) eun)

singleUsageLabels :: Label -> Body -> S.Set H.Label
-- | set of labels that are only used by a single goto
-- | excluding the entry label and continuations
singleUsageLabels entry body = suls `S.difference` (S.insert (hLabel entry) continuations)

  where
    suls :: S.Set H.Label
    suls  = S.fromList . map fst . filter ((==1) . snd) . M.toList . foldGraphNodes count body $ M.empty

    count (Label _)      m = m
    count (Cont _ _)     m = m
    count (Stmt  _)      m = m
    count n@(Goto _)     m = foldr (M.alter alter) m (successors n)
    count n@(If _ _ _ _) m = foldr (M.alter alter) m (successors n)
    count (Call _ _)     m = m
    count n@(Return _ _) m = foldr (M.alter alter) m (successors n)
--    count n m = foldr (M.alter alter) m (successors n) XXX why doesn't this work?
    alter Nothing  = Just (1::Int)
    alter (Just x) = Just (x+1)

    continuations :: S.Set H.Label
    continuations = foldGraphNodes count' body $ S.empty
    count' n@(Call _ _) s = foldr S.insert s (successors n) 
    count' _            s = s

lblIdent :: Label -> Symbol -> Ident -- {{{2
lblIdent lbl fname = ii $ case lbl of
  (TLabel l _) -> mangleFun l fname
  (ILabel l)   -> mangleFun (contLbl (show l)) fname

tstackAccess :: [Symbol] -> Maybe Symbol -> ENodeInfo -> CExpr' -- {{{2
tstackAccess (sf:chain) variable eni = foldl create base members
  where
    variables = maybeToList variable
    base = CVar (ii (tstackVar sf)) eni
    members = foldr (\x l -> frameUnion : x : l) [] chain ++ variables
    create inner member = CMember inner (ii  member) False eni
tstackAccess [] _ _ = $abort "called tstackAccess with empty call chain"

estackAccess :: Symbol -> Symbol -> ENodeInfo -> CExpr' -- {{{2
estackAccess function variable eni = CMember (CMember (CVar (ii estackVar) eni) (ii function) False eni) (ii variable) False eni

staticAccess :: Symbol -> ENodeInfo -> CExpr' -- {{{2
staticAccess variable eni = CVar (ii variable) eni

ii :: String -> Ident -- {{{2
ii = internalIdent
