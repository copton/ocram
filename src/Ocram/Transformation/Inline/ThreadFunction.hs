{-# LANGUAGE TemplateHaskell #-}
module Ocram.Transformation.Inline.ThreadFunction
-- exports {{{1
(
  addThreadFunctions
) where

-- imports {{{1
import Control.Monad (liftM)
import Control.Monad.State (get, put, State, runState)
import Data.Generics (everywhereM, everywhere, mkT, mkM)
import Data.Maybe (maybeToList)
import Language.C.Syntax.AST
import Ocram.Analysis (start_functions, call_chain, call_order, is_blocking, is_critical, CallGraph)
import Ocram.Query (function_definition, function_parameters, local_variables_fd)
import Ocram.Symbols (symbol, Symbol)
import Ocram.Transformation.Inline.Names
import Ocram.Transformation.Inline.Types
import Ocram.Transformation.Util (un, ident)
import Ocram.Types (Ast)
import Ocram.Util ((?:), fromJust_s, abort)
import Prelude hiding (exp, id)
import qualified Data.Map as Map
import qualified Data.Set as Set

addThreadFunctions :: Transformation -- {{{1
addThreadFunctions cg ast@(CTranslUnit decls ni) = do
  thread_functions <- mapM (liftM CFDefExt . createThreadFunction cg ast) $ zip [1..] $ Set.elems $ start_functions cg
  return $ CTranslUnit (decls ++ thread_functions) ni

createThreadFunction :: CallGraph -> Ast -> (Int, Symbol) -> WR CFunDef -- {{{2
createThreadFunction cg ast (tid, startFunction) =
  return $ CFunDef [CTypeSpec (CVoidType un)] (CDeclr (Just (ident (handlerFunction tid))) [CFunDeclr (Right ([CDecl [CTypeSpec (CVoidType un)] [(Just (CDeclr (Just (ident contVar)) [CPtrDeclr [] un] Nothing [] un), Nothing, Nothing)] un], False)) [] un] Nothing [] un) [] (CCompound [] (intro : concat functions) un) un
  where
    intro = CBlockStmt (CIf (CBinary CNeqOp (CVar (ident contVar) un) (CVar (ident "NULL") un) un) (CGotoPtr (CVar (ident contVar) un) un) Nothing un)
    onlyDefs name = not (is_blocking cg name) && is_critical cg name
    functions = map (inlineCriticalFunction cg ast startFunction) $ zip (True : repeat False) $ filter onlyDefs $ $fromJust_s $ call_order cg startFunction

inlineCriticalFunction :: CallGraph -> Ast -> Symbol -> (Bool, Symbol) -> [CBlockItem] -- {{{2
inlineCriticalFunction cg ast startFunction (isThreadStartFunction, inlinedFunction) = lbl ?: inlinedBody ++ close : []
  where
    callChain = $fromJust_s $ call_chain cg startFunction inlinedFunction
    fd = $fromJust_s $ function_definition ast inlinedFunction
    localVariables = local_variables_fd fd

    lbl = if isThreadStartFunction
      then Nothing
      else Just $ createLabel inlinedFunction 0

    close = CBlockStmt $ if startFunction == inlinedFunction
      then CReturn Nothing un
      else CGotoPtr (stackAccess callChain (Just contVar)) un

    inlinedBody = extractBody $ (rewriteCriticalFunctionCalls . rewriteLocalVariableAccess  . rewriteLocalVariableDecls) fd

    extractBody (CFunDef _ _ _ (CCompound _ body _) _) = body
    extractBody _ = $abort "unexpected parameters"

    rewriteLocalVariableAccess :: CFunDef -> CFunDef -- {{{3
    rewriteLocalVariableAccess = everywhere (mkT rewrite)
      where
        rewrite o@(CVar iden _)
          | Map.member name localVariables = stackAccess callChain (Just name)
          | otherwise = o
          where name = symbol iden
        rewrite o = o

    rewriteLocalVariableDecls :: CFunDef -> CFunDef  -- {{{3
    rewriteLocalVariableDecls = everywhere (mkT rewrite)
      where
        rewrite (CCompound x items y) = CCompound x (concatMap transform items) y
        rewrite o = o

        transform (CBlockDecl cd) = initialize cd
        transform o = [o]

        initialize cd@(CDecl _ [(_, Just(CInitExpr expr _), _)] _) =
          [CBlockStmt (CExpr (Just (CAssign CAssignOp (var cd) expr un)) un)]
        initialize _ = []

        var cd = stackAccess callChain (Just (symbol cd))

    rewriteCriticalFunctionCalls :: CFunDef -> CFunDef -- {{{3
    rewriteCriticalFunctionCalls fd' = fst $ runState (everywhereM (mkM rewrite) fd') 1
      where
        rewrite (CCompound x items y) = do
          items' <- mapM transform items
          return $ CCompound x (concat items') y
        rewrite o = return o

        transform o@(CBlockStmt (CExpr (Just (CCall (CVar iden _) params _)) _))
          | is_critical cg calledFunction = callSequence calledFunction params Nothing
          | otherwise = return [o]
          where calledFunction = symbol iden

        transform o@(CBlockStmt (CExpr (Just (CAssign CAssignOp lhs (CCall (CVar iden _) params _) _)) _))
          | is_critical cg calledFunction = callSequence calledFunction params (Just lhs)
          | otherwise = return [o]
          where calledFunction = symbol iden 

        transform o = return [o]

        callSequence calledFunction params resultLhs = do
          lblIdx <- get
          put (lblIdx + 1)  
          return $ criticalFunctionCallSequence calledFunction lblIdx params resultLhs

    criticalFunctionCallSequence :: Symbol -> Int -> [CExpr] -> Maybe CExpr -> [CBlockItem] -- {{{3
    criticalFunctionCallSequence calledFunction lblIdx params resultLhs =
      parameters ++ continuation : callExp : returnExp ?: lbl' : resultExp ?: []
      where
        callChain' = callChain ++ [calledFunction]
        blocking = is_blocking cg calledFunction
        parameters = zipWith createParamAssign params $ $fromJust_s $ function_parameters ast calledFunction
        continuation = CBlockStmt (CExpr (Just (CAssign CAssignOp (stackAccess callChain' (Just contVar)) (CLabAddrExpr (ident $ label inlinedFunction lblIdx) un) un)) un)
        lbl' = createLabel inlinedFunction lblIdx
        resultExp = fmap assignResult resultLhs
        assignResult lhs = createAssign lhs (stackAccess callChain' (Just resVar))

        callExp = CBlockStmt $ if blocking
          then CExpr (Just (CCall (CVar (ident calledFunction) un) [CUnary CAdrOp (stackAccess callChain' Nothing) un] un)) un
          else CGoto (ident $ label calledFunction 0) un

        returnExp = if blocking
          then Just (CBlockStmt (CReturn Nothing un))
          else Nothing

        createParamAssign exp decl = createAssign (stackAccess callChain' (Just (symbol decl))) exp

        createAssign lhs rhs = CBlockStmt (CExpr (Just (CAssign CAssignOp lhs rhs un)) un)

stackAccess :: [Symbol] -> Maybe Symbol -> CExpr -- {{{2
stackAccess (sf:chain) variable = foldl create base members
  where
    variables = maybeToList variable
    base = CVar (ident $ stackVar sf) un
    members = foldr (\x l -> frameUnion : x : l) [] chain ++ variables
    create inner member = CMember inner (ident member) False un 
stackAccess [] _ = $abort "called stackAccess with empty call chain"

createLabel :: Symbol -> Int -> CBlockItem -- {{{2
createLabel name id = CBlockStmt $ CLabel (ident (label name id)) (CExpr Nothing un) [] un

