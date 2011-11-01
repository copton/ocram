{-# LANGUAGE TemplateHaskell #-}
-- add thread functions
module Ocram.Transformation.Inline.ThreadFunction
-- exports {{{1
(
  addThreadFunctions
) where

-- imports {{{1
import Control.Monad.Reader (ask)
import Control.Monad (liftM)
import Data.Maybe (isJust, fromJust, catMaybes)
import Data.Monoid (mempty)
import Language.C.Syntax.AST
import Ocram.Analysis (start_functions, call_chain, call_order, is_blocking, is_critical, CallGraph)
import Ocram.Query (function_definition, function_parameters, local_variables_fd, SymbolTable, unlist_decl)
import Ocram.Symbols (symbol)
import Ocram.Transformation.Inline.Names
import Ocram.Transformation.Inline.Types
import Ocram.Transformation.Util (un, ident)
import Ocram.Types (Ast)
import Ocram.Symbols (Symbol)
import Ocram.Util ((?:), fromJust_s, abort)
import Ocram.Visitor
import Prelude hiding (exp, id)
import qualified Data.Map as Map
import qualified Data.Set as Set

addThreadFunctions :: Ast -> WR Ast -- {{{1
addThreadFunctions ast@(CTranslUnit decls ni) = do
  cg <- ask
  thread_functions <- mapM (liftM CFDefExt . createThreadFunction ast) $ zip [1..] $ Set.elems $ start_functions cg
  return $ CTranslUnit (decls ++ thread_functions) ni

createThreadFunction :: Ast -> (Int, Symbol) -> WR CFunDef -- {{{2
createThreadFunction ast (tid, startFunction) = do
  cg <- ask
  let intro = CBlockStmt (CIf (CBinary CNeqOp (CVar (ident contVar) un) (CVar (ident "null") un) un) (CGotoPtr (CVar (ident contVar) un) un) Nothing un)
  let onlyDefs name = (not $ is_blocking cg name) && (is_critical cg name)
  functions <- mapM (inlineCriticalFunction ast startFunction) $ filter onlyDefs $ $fromJust_s $ call_order cg startFunction
  return $ CFunDef [CTypeSpec (CVoidType un)] (CDeclr (Just (ident (handlerFunction tid))) [CFunDeclr (Right ([CDecl [CTypeSpec (CVoidType un)] [(Just (CDeclr (Just (ident contVar)) [CPtrDeclr [] un] Nothing [] un), Nothing, Nothing)] un], False)) [] un] Nothing [] un) [] (CCompound [] (intro : concat functions) un) un

inlineCriticalFunction :: Ast -> Symbol -> Symbol -> WR [CBlockItem] -- {{{2
inlineCriticalFunction ast startFunction inlinedFunction = do
  cg <- ask
  let fd = $fromJust_s $ function_definition ast inlinedFunction
  let currentBody = (\(CFunDef _ _ _ x _) -> x) fd
  let localVariables = local_variables_fd fd
  let initialDownState = DownState cg ast startFunction inlinedFunction localVariables 1
  let inlinedBody = extractBody $ fst $ (traverseCStat currentBody initialDownState :: (CStat, UpState)) 
  let callChain = $fromJust_s $ call_chain cg startFunction inlinedFunction
  return $ lbl : inlinedBody ++ (close callChain) : []
    where
      close callChain = CBlockStmt $ if startFunction == inlinedFunction
        then CReturn Nothing un
        else CGotoPtr (stackAccess callChain (Just contVar)) un
      lbl = createLabel inlinedFunction 0
      extractBody (CCompound _ body _) = body
      extractBody _ = $abort "unexpected parameters"

data DownState = DownState {
    dCg :: CallGraph
  , dAst :: Ast
  , dSf :: Symbol -- start function
  , dIf :: Symbol -- inlined function
  , dSt :: SymbolTable -- local variables
  , dLbl :: Int
  }

type UpState = ()

instance DownVisitor DownState

instance UpVisitor DownState UpState where
  -- rewrite access to local variables
  upCExpr o@(CVar iden _) d  _
    | Map.member name (dSt d) = (stackAccess callChain (Just name), mempty)
    | otherwise = (o, mempty)
    where
      name = symbol iden
      callChain = $fromJust_s $ call_chain (dCg d) (dSf d) (dIf d)

  upCExpr o _ _ = (o, mempty)

instance ListVisitor DownState UpState where
  -- rewrite critical function calls
  nextCBlockItem o@(CBlockStmt (CExpr (Just (CCall (CVar iden _) params _)) _)) d u
    | is_critical (dCg d) calledFunction =
        (criticalFunctionCall d calledFunction params Nothing, nextLabel d, u)
    | otherwise = ([o], d, u)
    where
      calledFunction = symbol iden

  nextCBlockItem o@(CBlockStmt (CExpr (Just (CAssign CAssignOp lhs (CCall (CVar fName _) params _) _)) _)) d u
    | is_critical (dCg d) calledFunction = (criticalFunctionCall d calledFunction params (Just lhs), nextLabel d, u)
    | otherwise = ([o], d, u)
    where
      calledFunction = symbol fName

  -- remove/rewrite local variable declarations
  nextCBlockItem (CBlockDecl cd) d u = (os, d, u)
    where
      os = catMaybes $ map initialize $ unlist_decl cd
      initialize cd'@(CDecl _ [(_, Just(CInitExpr expr _), _)] _) = Just $
        CBlockStmt (CExpr (Just (CAssign CAssignOp (var cd') expr un)) un)
      initialize _ = Nothing
      var cd' = stackAccess callChain (Just (symbol cd'))
      callChain = $fromJust_s $ call_chain (dCg d) (dSf d) (dIf d)

  nextCBlockItem o d u = ([o], d, u)

criticalFunctionCall :: DownState -> Symbol -> [CExpr] -> Maybe CExpr -> [CBlockItem]
criticalFunctionCall d calledFunction params resultLhs =
  parameters ++ continuation : callExp : returnExp ?: lbl : resultExp ?: []
  where
    blocking = is_blocking (dCg d) calledFunction
    callChain = $fromJust_s $ call_chain (dCg d) (dSf d) calledFunction
    parameters = map (createParamAssign callChain) $ zip params $ $fromJust_s $ function_parameters (dAst d) calledFunction
    continuation = CBlockStmt (CExpr (Just (CAssign CAssignOp (stackAccess callChain (Just contVar)) (CUnary CAdrOp (CVar (ident $ label (dIf d) (dLbl d)) un) un) un)) un)
    lbl = createLabel (dIf d) (dLbl d)
    resultExp = fmap assignResult resultLhs
    assignResult lhs = createAssign lhs (stackAccess callChain (Just resVar))

    callExp = CBlockStmt $ if blocking
      then CExpr (Just (CCall (CVar (ident calledFunction) un) [CUnary CAdrOp (stackAccess callChain Nothing) un] un)) un
      else CGoto (ident $ label calledFunction 0) un

    returnExp = if blocking
      then Just (CBlockStmt (CReturn Nothing un))
      else Nothing


createParamAssign :: [Symbol] -> (CExpr, CDecl) -> CBlockItem
createParamAssign chain (exp, decl) = createAssign lhs rhs
  where
    lhs = stackAccess chain $ Just $ symbol decl
    rhs = exp

createAssign :: CExpr -> CExpr -> CBlockItem
createAssign lhs rhs = CBlockStmt (CExpr (Just (CAssign CAssignOp lhs rhs un)) un)

stackAccess :: [Symbol] -> Maybe Symbol -> CExpr
stackAccess (sf:chain) variable = foldl create base members
  where
    variables = if isJust variable then [fromJust variable] else []
    base = CVar (ident $ stackVar sf) un
    members = foldr (\x l -> frameUnion : x : l) [] chain ++ variables
    create inner member = CMember inner (ident member) False un 
stackAccess [] _ = $abort "called stackAccess with empty call chain"

createLabel :: Symbol -> Int -> CBlockItem
createLabel name id = CBlockStmt $ CLabel (ident (label name id)) (CExpr Nothing un) [] un

nextLabel :: DownState -> DownState
nextLabel d = d {dLbl = (dLbl d) + 1}
