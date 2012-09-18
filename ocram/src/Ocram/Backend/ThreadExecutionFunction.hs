{-# LANGUAGE TemplateHaskell, GADTs #-}
module Ocram.Backend.ThreadExecutionFunction
-- exports {{{1
(
  thread_execution_functions
) where

-- imports {{{1
import Compiler.Hoopl (postorder_dfs, foldBlockNodesF, Block(..), C, (|*><*|), mkLast)
import Data.Generics (everywhere, mkT)
import Data.Maybe (mapMaybe, maybeToList)
import Language.C.Data.Ident (Ident, internalIdent)
import Language.C.Data.Node (undefNode, NodeInfo)
import Language.C.Syntax.AST
import Ocram.Analysis (CallGraph, call_order, start_functions, call_chain, is_blocking)
import Ocram.Query (function_parameters_fd)
import Ocram.Intermediate
import Ocram.Names (mangleFun, contLbl, tfunction, contVar, frameUnion, tstackVar, resVar, estackVar)
import Ocram.Symbols (Symbol, symbol)
import Ocram.Util ((?:), fromJust_s, abort)

import qualified Data.Map as M

thread_execution_functions :: CallGraph -> M.Map Symbol Function -> M.Map Symbol (Maybe CDecl) -> [CFunDef] -- {{{1
thread_execution_functions cg cf estacks = map tef $ zip [1..] (start_functions cg)
  where
    tef (tid, sf) = threadExecutionFunction cg cf ($fromJust_s (M.lookup sf estacks)) tid sf

threadExecutionFunction :: CallGraph -> M.Map Symbol Function -> Maybe CDecl -> Int -> Symbol -> CFunDef -- {{{2
threadExecutionFunction cg cf estack tid name = fun
  where
    fun =
      CFunDef [CTypeSpec (CVoidType un)] (CDeclr (Just (ii (tfunction tid))) [fdeclr] Nothing [] un) [] body un

    fdeclr =
      CFunDeclr (Right ([CDecl [CTypeSpec (CVoidType un)] [(Just (CDeclr (Just (ii contVar)) [CPtrDeclr [] un] Nothing [] un), Nothing, Nothing)] un], False)) [] un

    body =
      CCompound [] (fmap CBlockDecl estack ?: intro : functions) un

    intro =
      CBlockStmt (CIf (CVar (ii contVar) un) (CGotoPtr (CVar (ii contVar) un) un) Nothing un)

    functions = map CBlockStmt . concatMap (inlineCriticalFunction cg cf name) . mapMaybe (flip M.lookup cf) . $fromJust_s . call_order cg $ name

inlineCriticalFunction :: CallGraph -> M.Map Symbol Function -> Symbol -> Function -> [CStat] -- {{{2
inlineCriticalFunction cg cf startFunction inlinedFunction = concatMap convertBlock (postorder_dfs (mkLast (Goto (fun_entry inlinedFunction)) |*><*| fun_body inlinedFunction))
  where
    -- stuff {{{3
    name = fun_name inlinedFunction
    callChain = $fromJust_s $ call_chain cg startFunction name

    convertBlock :: Block Node C C -> [CStat] -- {{{3
    convertBlock block = reverse $ foldBlockNodesF convertNode block []

    convertNode :: Node e x -> [CStat] -> [CStat] -- {{{3
    convertNode (Label lbl)     ss = CLabel (lblIdent lbl name) (CExpr Nothing un) [] un : ss
    convertNode (Stmt expr)     ss = CExpr (Just (rewriteLocalVariableAccess expr)) un : ss
    convertNode (Goto lbl)      ss = CGoto (lblIdent lbl name) un : ss
    convertNode (If cond tl el) ss = CIf (rewriteLocalVariableAccess cond) (CGoto (lblIdent tl name) un) (Just (CGoto (lblIdent el name) un)) un : ss

    convertNode (Call (FirstNormalForm callee params) lbl) ss = 
      criticalCallSequence callee (map rewriteLocalVariableAccess params) lbl Nothing ++ ss

    convertNode (Call (SecondNormalForm lhs op callee params) lbl) ss =
      criticalCallSequence callee (map rewriteLocalVariableAccess params) lbl (Just (rewriteLocalVariableAccess lhs, op)) ++ ss

    convertNode (Return expr) ss
      | startFunction == fun_name inlinedFunction = CReturn Nothing un : ss
      | otherwise = case expr of
          Nothing    -> CGotoPtr (tstackAccess callChain (Just contVar) un) un : ss
          Just expr' -> 
              CExpr (Just (CAssign CAssignOp (tstackAccess callChain (Just resVar) un) expr' un)) un
            : CGotoPtr (tstackAccess callChain (Just contVar) un) un
            : ss 

    criticalCallSequence callee params lbl nf2 = -- {{{3
      parameters ++ continuation : callExp : returnExp ?: label : resultExpr ?: []
      where
        callChain' = callChain ++ [callee]
        blocking   = is_blocking cg callee
        fun        = $fromJust_s $ M.lookup callee cf

        parameters = zipWith paramAssign params $ function_parameters_fd (fun_def fun)

        continuation = assign (tstackAccess callChain' (Just contVar) un) (CLabAddrExpr (lblIdent lbl callee) un)

        callExp
          | blocking  = CExpr (Just (CCall (CVar (ii callee) un) [CUnary CAdrOp (tstackAccess callChain' Nothing un) un] un)) un 
          | otherwise = CGoto (lblIdent (fun_entry fun) callee) un

        returnExp
          | blocking  = Just $ CReturn Nothing un
          | otherwise = Nothing

        label = CLabel (lblIdent lbl callee) (CExpr Nothing un) [] un

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
          | test fun_cVars  = tstackAccess callChain (Just name) un
          | test fun_ncVars = estackAccess startFunction name
          | otherwise       = o
          where
            name = symbol iden
            test f = name `elem` map var_fqn (f inlinedFunction)
        rewrite o = o

lblIdent :: Label -> Symbol -> Ident -- {{{2
lblIdent lbl fname = ii $ case lbl of
  (TLabel l _) -> mangleFun l fname
  (ILabel l)   -> mangleFun (contLbl (show l)) fname

tstackAccess :: [Symbol] -> Maybe Symbol -> NodeInfo -> CExpr -- {{{2
tstackAccess (sf:chain) variable ni = foldr create base members
  where
    variables = maybeToList variable
    base = CVar (ii (tstackVar sf)) ni
    members = foldr (\x l -> frameUnion : x : l) [] chain ++ variables
    create member inner = CMember inner (ii  member) False ni
tstackAccess [] _ _ = $abort "called tstackAccess with empty call chain"

estackAccess :: Symbol -> Symbol -> CExpr -- {{{2
estackAccess function variable = CMember (CMember (CVar (ii estackVar) un) (ii function) False un) (ii variable) False un

un :: NodeInfo -- {{{2
un = undefNode

ii :: String -> Ident -- {{{2
ii = internalIdent
