{-# LANGUAGE TemplateHaskell, GADTs#-}
module Ocram.Backend.ThreadExecutionFunction
-- exports {{{1
(
  thread_execution_functions
) where

-- imports {{{1
import Compiler.Hoopl (postorder_dfs_from, foldGraphNodes, successors, entryLabel, O, C, mapLookup)
import Data.Generics (everywhere, mkT)
import Data.Maybe (mapMaybe, maybeToList)
import Language.C.Data.Ident (Ident, internalIdent)
import Language.C.Data.Node (undefNode, NodeInfo)
import Language.C.Syntax.AST
import Ocram.Analysis (CallGraph, call_order, start_functions, call_chain, is_blocking)
import Ocram.Query (function_parameters_fd, function_parameters_cd)
import Ocram.Intermediate
import Ocram.Names (mangleFun, contLbl, tfunction, contVar, frameUnion, tstackVar, resVar, estackVar)
import Ocram.Symbols (Symbol, symbol)
import Ocram.Util ((?:), fromJust_s, abort, lookup_s)
import Prelude hiding (last)

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Compiler.Hoopl as H

thread_execution_functions :: CallGraph -> M.Map Symbol CDecl -> M.Map Symbol Function -> M.Map Symbol (Maybe CDecl) -> [CFunDef] -- {{{1
thread_execution_functions cg bf cf estacks = map tef $ zip [0..] (start_functions cg)
  where
    tef (tid, sf) = threadExecutionFunction cg bf cf ($lookup_s estacks sf) tid sf

threadExecutionFunction :: CallGraph -> M.Map Symbol CDecl -> M.Map Symbol Function -> Maybe CDecl -> Int -> Symbol -> CFunDef -- {{{2
threadExecutionFunction cg bf cf estack tid name = fun
  where
    fun =
      CFunDef [CTypeSpec (CVoidType un)] (CDeclr (Just (ii (tfunction tid))) [fdeclr] Nothing [] un) [] body un

    fdeclr =
      CFunDeclr (Right ([CDecl [CTypeSpec (CVoidType un)] [(Just (CDeclr (Just (ii contVar)) [CPtrDeclr [] un] Nothing [] un), Nothing, Nothing)] un], False)) [] un

    body =
      CCompound [] (fmap CBlockDecl estack ?: intro : functions) un

    intro =
      CBlockStmt (CIf (CVar (ii contVar) un) (CGotoPtr (CVar (ii contVar) un) un) Nothing un)

    functions = concatMap (inlineCriticalFunction cg callees entries name) . mapMaybe (flip M.lookup cf) . $fromJust_s . call_order cg $ name

    callees = M.map function_parameters_cd bf `M.union` M.map (function_parameters_fd . fun_def) cf
    entries = M.map fun_entry cf

inlineCriticalFunction :: CallGraph -> M.Map Symbol [CDecl] -> M.Map Symbol Label -> Symbol -> Function -> [CBlockItem] -- {{{2
inlineCriticalFunction cg callees entries startFunction inlinedFunction = cGraph blocks
  where
    fname     = fun_name inlinedFunction
    callChain = $fromJust_s $ call_chain cg startFunction fname
    suls      = singleUsageLabels (fun_entry inlinedFunction) (fun_body inlinedFunction)
    blocks    = block_map $ fun_body inlinedFunction

    cGraph :: BlockMap -> [CBlockItem]
    cGraph lm =
      let entry = Goto (fun_entry inlinedFunction) in
        concatMap cBlock
      $ filter (not . flip S.member suls . entryLabel)
      $ postorder_dfs_from lm entry

    cBlock :: Block -> [CBlockItem]
    cBlock block =
      let (Label entry, middles, last) = block_components block in
        CBlockStmt (CLabel (lblIdent entry fname) (CExpr Nothing un) [] un)
      : map cMiddle middles ++ cLast last

    inlineBlock :: Label -> [CBlockItem]
    inlineBlock lbl = 
      let
        block = $fromJust_s $ mapLookup (hLabel lbl) blocks
        (_, middles, last) = block_components block
      in
        map cMiddle middles ++ cLast last

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
         criticalCallSequence callee (map cExpr params) lbl Nothing
      ++ inlineBlock lbl

    cLast (Call (SecondNormalForm  lhs op callee params) lbl) =
         criticalCallSequence callee (map cExpr params) lbl (Just (cExpr lhs, op)) 
      ++ inlineBlock lbl

    cBranch lbl
      | S.member (hLabel lbl) suls = CCompound [] (inlineBlock lbl) un 
      | otherwise = CCompound [] [CBlockStmt (CGoto (lblIdent lbl fname) un)] un 

    cExpr = rewriteLocalVariableAccess

    criticalCallSequence callee params lbl nf2 = -- {{{3
      map CBlockStmt $ parameters ++ continuation : callExp : returnExp ?: label : resultExpr ?: []
      where
        callChain' = callChain ++ [callee]
        blocking   = is_blocking cg callee

        parameters = zipWith paramAssign params ($lookup_s callees callee)

        continuation = assign (tstackAccess callChain' (Just contVar) un) (CLabAddrExpr (lblIdent lbl fname) un)

        callExp
          | blocking  = CExpr (Just (CCall (CVar (ii callee) un) [CUnary CAdrOp (tstackAccess callChain' Nothing un) un] un)) un 
          | otherwise = CGoto (lblIdent ($lookup_s entries callee) callee) un

        label = CLabel (lblIdent lbl fname) (CExpr Nothing un) [] un

        returnExp
          | blocking  = Just $ CReturn Nothing un
          | otherwise = Nothing

        resultExpr = case nf2 of
          Nothing -> Nothing
          Just (lhs, op) -> Just $
            CExpr (Just (CAssign op lhs (tstackAccess callChain' (Just resVar) un) un)) un

        paramAssign e decl = assign (tstackAccess callChain' (Just (symbol decl)) un) e 
        assign lhs rhs = CExpr (Just (CAssign CAssignOp lhs rhs un)) un
    
    rewriteLocalVariableAccess :: CExpr -> CExpr -- {{{3
    rewriteLocalVariableAccess = everywhere (mkT rewrite)
      where
        rewrite :: CExpr -> CExpr
        rewrite o@(CVar iden _)
          | test fun_cVars  = tstackAccess callChain (Just vname) un
          | test fun_ncVars = estackAccess (fun_name inlinedFunction) vname
          | otherwise       = o
          where
            vname = symbol iden
            test f = vname `elem` map var_unique (f inlinedFunction)
        rewrite o = o

singleUsageLabels :: Label -> Body -> S.Set H.Label
singleUsageLabels entry body = M.keysSet $ M.filterWithKey (\_ v -> v == (1 :: Int)) $ foldGraphNodes count body $ M.singleton (hLabel entry) 2
  where
    count (Label _)    m = m
    count (Cont _ _)   m = m
    count (Stmt  _)    m = m
    count n@(Goto _)   m = foldr (M.alter alter) m (successors n)
    count n@(If _ _ _) m = foldr (M.alter alter) m (successors n)
    count n@(Call _ _) m = foldr (M.alter alter) m (successors n)
    count n@(Return _) m = foldr (M.alter alter) m (successors n)
--    count n m = foldr (M.alter alter) m (successors n) XXX why doesn't this work?
    alter Nothing  = Just 1
    alter (Just x) = Just (x+1)

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

un :: NodeInfo -- {{{2
un = undefNode

ii :: String -> Ident -- {{{2
ii = internalIdent
