-- add thread functions
module Ocram.Transformation.Inline.Step3
-- exports {{{1
(
  step3
) where

-- imports {{{1
import Control.Monad.Reader (ask)
import Control.Monad (liftM)
import Data.Maybe (isJust, fromJust)
import Data.Monoid (mempty)
import Language.C.Syntax.AST
import Ocram.Analysis (start_functions, call_chain, call_order, is_blocking, is_critical, CallGraph, function_info, FunctionInfo(..), SymTab)
import Ocram.Symbols (symbol)
import Ocram.Transformation.Inline.Names
import Ocram.Transformation.Inline.Types
import Ocram.Transformation.Util (un, ident)
import Ocram.Types (Ast)
import Ocram.Symbols (Symbol)
import Ocram.Util ((?:), fromJust_s)
import Ocram.Visitor
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List

step3 :: Ast -> WR Ast -- {{{1
step3 (CTranslUnit decls ni) = do
  cg <- ask
  thread_functions <- mapM (liftM CFDefExt . createThreadFunction) $ zip [1..] $ Set.elems $ start_functions cg
  return $ CTranslUnit (decls ++ thread_functions) ni

createThreadFunction :: (Integer, Symbol) -> WR CFunDef -- {{{2
createThreadFunction (tid, startFunction) = do
  cg <- ask
  let intro = CBlockStmt (CIf (CBinary CNeqOp (CVar (ident contVar) un) (CVar (ident "null") un) un) (CGotoPtr (CVar (ident contVar) un) un) Nothing un)
  let onlyDefs name = (not $ is_blocking cg name) && (is_critical cg name)
  functions <- mapM (inlineCriticalFunction startFunction) $ filter onlyDefs $ fromJust_s "Step3/7" $ call_order cg startFunction
  return $ CFunDef [CTypeSpec (CVoidType un)] (CDeclr (Just (ident (handlerFunction tid))) [CFunDeclr (Right ([CDecl [CTypeSpec (CVoidType un)] [(Just (CDeclr (Just (ident contVar)) [CPtrDeclr [] un] Nothing [] un), Nothing, Nothing)] un], False)) [] un] Nothing [] un) [] (CCompound [] (intro : concat functions) un) un

inlineCriticalFunction :: Symbol -> Symbol -> WR [CBlockItem] -- {{{2
inlineCriticalFunction startFunction inlinedFunction = do
  cg <- ask
  let lbl = createLabel inlinedFunction 0
  let fi = fromJust_s "Step3/1" $ function_info cg inlinedFunction
  let currentBody = fromJust_s "Step3/2" $ fiBody fi
  let initialDownState = DownState cg startFunction inlinedFunction (fiVariables fi) 1
  let inlinedBody = (\(CCompound _ body _) -> body) $ fst $ (traverseCStat currentBody initialDownState :: (CStat, UpState)) 
  let close = CBlockStmt $ CReturn Nothing un
  return $ lbl : inlinedBody ++ close : []

data DownState = DownState {
    dCg :: CallGraph
  , dSf :: Symbol -- start function
  , dIf :: Symbol -- inlined function
  , dSt :: SymTab -- local variables
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
      callChain = fromJust_s "Step3/6" $ call_chain (dCg d) (dSf d) (dIf d)

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

  nextCBlockItem o d u = ([o], d, u)

criticalFunctionCall :: DownState -> Symbol -> [CExpr] -> Maybe CExpr -> [CBlockItem]
criticalFunctionCall (DownState cg startFunction inlinedFunction _ lid) calledFunction params resultLhs =
  parameters ++ continuation : call : return : lbl : result ?: []
  where
    callChain = fromJust_s "Step3/8" $ call_chain cg startFunction calledFunction
    fi = fromJust_s "Step3/5" $ function_info cg calledFunction
    parameters = map (createParamAssign callChain) $ zip params $ fiParams fi
    continuation = CBlockStmt (CExpr (Just (CAssign CAssignOp (stackAccess callChain (Just contVar)) (CUnary CAdrOp (CVar (ident $ label inlinedFunction lid) un) un) un)) un)
    call = CBlockStmt (CExpr (Just (CCall (CVar (ident calledFunction) un) [CUnary CAdrOp (stackAccess callChain Nothing) un] un)) un)
    return = CBlockStmt (CReturn Nothing un)
    lbl = createLabel inlinedFunction lid
    result = fmap assignResult resultLhs
    assignResult lhs = createAssign lhs (stackAccess callChain (Just resVar))

createParamAssign :: [Symbol] -> (CExpr, CDecl) -> CBlockItem
createParamAssign chain (exp, decl) = createAssign lhs rhs
  where
    lhs = stackAccess chain $ Just $ symbol decl
    rhs = exp

createAssign :: CExpr -> CExpr -> CBlockItem
createAssign lhs rhs = CBlockStmt (CExpr (Just (CAssign CAssignOp lhs rhs un)) un)

stackAccess :: [Symbol] -> Maybe Symbol -> CExpr
stackAccess (sf:chain) variable = foldl create base $ zip pointers members
	where
		variables = if isJust variable then [fromJust variable] else []
		base = CVar (ident $ stackVar sf) un
		pointers = True : cycle [False]
		members = foldr (\x l -> frameUnion : x : l) [] chain ++ variables
		create inner (pointer, member) = CMember inner (ident member) pointer un 

createLabel name id = CBlockStmt $ CLabel (ident (label name id)) (CExpr Nothing un) [] un

nextLabel d = d {dLbl = (dLbl d) + 1}
