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
import Language.C.Data.Node (undefNode, NodeInfo)
import Language.C.Syntax.AST
import Ocram.Analysis (CallGraph, call_order, start_functions, call_chain, is_blocking)
import Ocram.Query (function_parameters_fd, function_parameters_cd)
import Ocram.Intermediate
import Ocram.Debug (VarMap')
import Ocram.Print (render)
import Ocram.Ruab (Variable(AutomaticVariable), FQN)
import Ocram.Names (mangleFun, contLbl, tfunction, contVar, frameUnion, tstackVar, resVar, estackVar)
import Ocram.Symbols (Symbol, symbol)
import Ocram.Util ((?:), fromJust_s, abort, lookup_s)
import Prelude hiding (last)

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Compiler.Hoopl as H

thread_execution_functions :: CallGraph -> M.Map Symbol CDecl -> M.Map Symbol Function -> M.Map Symbol (Maybe CDecl) -> ([CFunDef], VarMap') -- {{{1
thread_execution_functions cg bf cf estacks = second concat $ unzip $ zipWith tef [0..] (start_functions cg)
  where
    tef tid sf = threadExecutionFunction cg bf cf ($lookup_s estacks sf) tid sf

threadExecutionFunction :: CallGraph -> M.Map Symbol CDecl -> M.Map Symbol Function -> Maybe CDecl -> Int -> Symbol -> (CFunDef, VarMap') -- {{{2
threadExecutionFunction cg bf cf estack tid name = (fun, concat vms)
  where
    fun =
      CFunDef [CTypeSpec (CVoidType un)] (CDeclr (Just (ii (tfunction tid))) [fdeclr] Nothing [] un) [] body un

    fdeclr =
      CFunDeclr (Right ([CDecl [CTypeSpec (CVoidType un)] [(Just (CDeclr (Just (ii contVar)) [CPtrDeclr [] un] Nothing [] un), Nothing, Nothing)] un], False)) [] un

    body =
      CCompound [] (fmap CBlockDecl estack ?: intro : concat functions) un

    intro =
      CBlockStmt (CIf (CVar (ii contVar) un) (CGotoPtr (CVar (ii contVar) un) un) Nothing un)

    (functions, vms) =  unzip . map (inlineCriticalFunction cg tid callees entries name) . mapMaybe (flip M.lookup cf) . $fromJust_s . call_order cg $ name

    callees = M.map function_parameters_cd bf `M.union` M.map (function_parameters_fd . fun_def) cf
    entries = M.map fun_entry cf

inlineCriticalFunction :: CallGraph -> Int -> M.Map Symbol [CDecl] -> M.Map Symbol Label -> Symbol -> Function -> ([CBlockItem], VarMap') -- {{{2
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

    cGraph :: BlockMap -> [CBlockItem]
    cGraph lm =
      let entry = Goto (fun_entry inlinedFunction) in
        concatMap cBlock
      $ filter (not . flip S.member suls . entryLabel)
      $ postorder_dfs_from lm entry

    cBlock :: Block -> [CBlockItem]
    cBlock block =
      let (first, middles, last) = block_components block in
        cFirst first ++ map cMiddle middles ++ cLast last

    inlineBlock :: Label -> [CBlockItem]
    inlineBlock lbl = 
      let
        block = $fromJust_s $ mapLookup (hLabel lbl) blocks
        (_, middles, last) = block_components block
      in
        map cMiddle middles ++ cLast last

    cFirst :: Node C O -> [CBlockItem]
    cFirst (Label entry) = [CBlockStmt (CLabel (lblIdent entry fname) (CExpr Nothing un) [] un)]

    cFirst (Cont lbl (FirstNormalForm _ _)) = cFirst (Label lbl)
    cFirst (Cont lbl (SecondNormalForm lhs op callee _)) =
      let 
        callChain' = callChain ++ [callee] 
        lhs'       = rewriteLocalVariableAccess lhs
        resultExpr = CExpr (Just (CAssign op lhs' (tstackAccess callChain' (Just resVar) un) un)) un
      in cFirst (Label lbl) ++ [CBlockStmt resultExpr]

    cMiddle :: Node O O -> CBlockItem
    cMiddle (Stmt expr) = CBlockStmt $ CExpr (Just (cExpr expr)) un 
    
    cLast :: Node O C -> [CBlockItem]

    cLast (Return mexpr)
      | startFunction == fname = [CBlockStmt (CReturn (fmap cExpr mexpr) un)]
      | otherwise              = map CBlockStmt $ 
           maybeToList (fmap assignResult mexpr)
        ++ [CGotoPtr (tstackAccess callChain (Just contVar) un) un]
      where assignResult expr = CExpr (Just (CAssign CAssignOp (tstackAccess callChain (Just resVar) un) (cExpr expr) un)) un

    cLast (Goto lbl)
      | S.member (hLabel lbl) suls = inlineBlock lbl
      | otherwise                  = [CBlockStmt $ CGoto (lblIdent lbl fname) un]

    cLast (If cond t e) = [CBlockStmt $ CIf (cExpr cond) (cBranch t) (Just (cBranch e)) un]

    cLast (Call (FirstNormalForm callee params) lbl) =
         criticalCallSequence callee (map cExpr params) lbl

    cLast (Call (SecondNormalForm  _ _ callee params) lbl) =
         criticalCallSequence callee (map cExpr params) lbl

    cBranch lbl
      | S.member (hLabel lbl) suls = CCompound [] (inlineBlock lbl) un 
      | otherwise = CCompound [] [CBlockStmt (CGoto (lblIdent lbl fname) un)] un 

    cExpr :: CExpr -> CExpr
    cExpr = everywhere (mkT rewriteLocalVariableAccess)

    criticalCallSequence callee params lbl = -- {{{3
      map CBlockStmt $ parameters ++ continuation : callExp : returnExp ?: []
      where
        callChain' = callChain ++ [callee]
        blocking   = is_blocking cg callee

        parameters = zipWith paramAssign params ($lookup_s callees callee)

        continuation = assign (tstackAccess callChain' (Just contVar) un) (CLabAddrExpr (lblIdent lbl fname) un)

        callExp
          | blocking  = CExpr (Just (CCall (CVar (ii callee) un) [CUnary CAdrOp (tstackAccess callChain' Nothing un) un] un)) un 
          | otherwise = CGoto (lblIdent ($lookup_s entries callee) callee) un

        returnExp
          | blocking  = Just $ CReturn Nothing un
          | otherwise = Nothing

        paramAssign e decl = assign (tstackAccess callChain' (Just (symbol decl)) un) e 
        assign lhs rhs = CExpr (Just (CAssign CAssignOp lhs rhs un)) un

    rewriteLocalVariableAccess :: CExpr -> CExpr -- {{{3
    rewriteLocalVariableAccess o@(CVar iden _)
      | test fun_cVars  = tstackAccess callChain (Just vname) un
      | test fun_ncVars = estackAccess (fun_name inlinedFunction) vname
      | test fun_stVars = staticAccess vname
      | otherwise       = o
      where
        vname = symbol iden
        test f = vname `elem` map var_unique (f inlinedFunction)
    rewriteLocalVariableAccess o = o

    symbol2fqn :: Symbol -> FQN -- {{{3
    symbol2fqn name =
      render $ rewriteLocalVariableAccess (CVar (internalIdent name) undefNode)

singleUsageLabels :: Label -> Body -> S.Set H.Label
-- | set of labels that are only used by a single goto
-- | excluding the entry label and continuations
singleUsageLabels entry body = suls `S.difference` (S.insert (hLabel entry) continuations)

  where
    suls :: S.Set H.Label
    suls  = S.fromList . map fst . filter ((==1) . snd) . M.toList . foldGraphNodes count body $ M.empty

    count (Label _)    m = m
    count (Cont _ _)   m = m
    count (Stmt  _)    m = m
    count n@(Goto _)   m = foldr (M.alter alter) m (successors n)
    count n@(If _ _ _) m = foldr (M.alter alter) m (successors n)
    count (Call _ _)   m = m
    count n@(Return _) m = foldr (M.alter alter) m (successors n)
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

tstackAccess :: [Symbol] -> Maybe Symbol -> NodeInfo -> CExpr -- {{{2
tstackAccess (sf:chain) variable ni = foldl create base members
  where
    variables = maybeToList variable
    base = CVar (ii (tstackVar sf)) ni
    members = foldr (\x l -> frameUnion : x : l) [] chain ++ variables
    create inner member = CMember inner (ii  member) False ni
tstackAccess [] _ _ = $abort "called tstackAccess with empty call chain"

estackAccess :: Symbol -> Symbol -> CExpr -- {{{2
estackAccess function variable = CMember (CMember (CVar (ii estackVar) un) (ii function) False un) (ii variable) False un

staticAccess :: Symbol -> CExpr  -- {{{2
staticAccess variable = CVar (ii variable) un

un :: NodeInfo -- {{{2
un = undefNode

ii :: String -> Ident -- {{{2
ii = internalIdent
