{-# LANGUAGE TemplateHaskell, GADTs #-}
module Ocram.Backend.ThreadExecutionFunction
-- exports {{{1
(
  thread_execution_functions
) where

-- imports {{{1
import Compiler.Hoopl (postorder_dfs, foldBlockNodesF, (|*><*|), mkLast)
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

import qualified Data.Map as M

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

    functions = map CBlockStmt . concatMap (inlineCriticalFunction cg bf cf name) . mapMaybe (flip M.lookup cf) . $fromJust_s . call_order cg $ name

inlineCriticalFunction :: CallGraph -> M.Map Symbol CDecl -> M.Map Symbol Function -> Symbol -> Function -> [CStat] -- {{{2
inlineCriticalFunction cg bf cf startFunction inlinedFunction =
  let graph = mkLast (Goto (fun_entry inlinedFunction)) |*><*| fun_body inlinedFunction in
  reverse $ fst $ foldl convertBlock ([],[]) (postorder_dfs graph)
  where
    -- stuff {{{3
    name = fun_name inlinedFunction
    callChain = $fromJust_s $ call_chain cg startFunction name

    convertBlock ctx block = foldBlockNodesF convertNode block ctx

    append x = append' [x]
    append' xs (ys, carry) = (carry ++ xs ++ ys, [])

    convertNode :: Node e x -> ([CStat], [CStat]) -> ([CStat], [CStat]) -- {{{3
    convertNode (Label lbl)     = append $ CLabel (lblIdent lbl name) (CExpr Nothing un) [] un
    convertNode (Stmt expr)     = append $ CExpr (Just (rewriteLocalVariableAccess expr)) un
    convertNode (Goto lbl)      = append $ CGoto (lblIdent lbl name) un
    convertNode (If cond tl el) = append $ CIf (rewriteLocalVariableAccess cond) (CGoto (lblIdent tl name) un) (Just (CGoto (lblIdent el name) un)) un

    convertNode (Call (FirstNormalForm callee params) lbl) =
      criticalCallSequence callee (map rewriteLocalVariableAccess params) lbl Nothing

    convertNode (Call (SecondNormalForm lhs op callee params) lbl) =
      criticalCallSequence callee (map rewriteLocalVariableAccess params) lbl (Just (rewriteLocalVariableAccess lhs, op))

    convertNode (Return expr)
      | startFunction == fun_name inlinedFunction = append $ CReturn Nothing un
      | otherwise = case expr of
          Nothing    -> append $ CGotoPtr (tstackAccess callChain (Just contVar) un) un
          Just expr' -> append' $ reverse [
              CExpr (Just (CAssign CAssignOp (tstackAccess callChain (Just resVar) un) (rewriteLocalVariableAccess expr') un)) un
            , CGotoPtr (tstackAccess callChain (Just contVar) un) un
            ]

    criticalCallSequence callee params lbl nf2 (ss, carry) = -- {{{3
      (carry ++ reverse (parameters ++ continuation : callExp : returnExp ?: []) ++ ss, resultExpr)
      where
        callChain' = callChain ++ [callee]
        blocking   = is_blocking cg callee
        sf         = M.map function_parameters_cd bf `M.union` M.map (function_parameters_fd . fun_def) cf

        parameters = zipWith paramAssign params ($lookup_s sf callee)

        continuation = assign (tstackAccess callChain' (Just contVar) un) (CLabAddrExpr (lblIdent lbl name) un)

        callExp
          | blocking  = CExpr (Just (CCall (CVar (ii callee) un) [CUnary CAdrOp (tstackAccess callChain' Nothing un) un] un)) un 
          | otherwise = CGoto (lblIdent (fun_entry ($lookup_s cf callee)) callee) un

        returnExp
          | blocking  = Just $ CReturn Nothing un
          | otherwise = Nothing

        resultExpr = case nf2 of
          Nothing -> []
          Just (lhs, op) -> 
            [CExpr (Just (CAssign op lhs (tstackAccess callChain' (Just resVar) un) un)) un]

        paramAssign e decl = assign (tstackAccess callChain' (Just (symbol decl)) un) e 
        assign lhs rhs = CExpr (Just (CAssign CAssignOp lhs rhs un)) un
    
    rewriteLocalVariableAccess :: CExpr -> CExpr -- {{{3
    rewriteLocalVariableAccess = everywhere (mkT rewrite)
      where
        rewrite :: CExpr -> CExpr
        rewrite o@(CVar iden _)
          | test fun_cVars  = tstackAccess callChain (Just vname) un
          | test fun_ncVars = estackAccess startFunction vname
          | otherwise       = o
          where
            vname = symbol iden
            test f = vname `elem` map var_fqn (f inlinedFunction)
        rewrite o = o

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
