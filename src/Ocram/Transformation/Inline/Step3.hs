-- add thread functions
module Ocram.Transformation.Inline.Step3
-- exports {{{1
(
  step3
) where

-- imports {{{1
import Control.Monad.Reader (ask)
import Data.Maybe (fromJust, isJust)
import Data.Monoid (mempty)
import Language.C.Syntax.AST
import Ocram.Analysis (start_functions, call_chain, call_order, is_blocking, is_critical)
import Ocram.Symbols (symbol)
import Ocram.Transformation.Inline.Names
import Ocram.Transformation.Inline.Types
import Ocram.Transformation.Util (un, ident)
import Ocram.Types (Ast)
import Ocram.Symbols (Symbol)
import Ocram.Util ((?:))
import Ocram.Visitor
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List

-- step3 :: Ast -> WR Ast {{{1
step3 :: Ast -> WR Ast
step3 (CTranslUnit decls ni) = do
  cg <- ask
  let sf = start_functions cg
  thread_functions <- mapM createThreadFunction $ zip [1..] $ Set.elems sf
  let thread_functions' = map CFDefExt thread_functions
  return $ CTranslUnit (decls ++ thread_functions') ni

-- createThreadFunction :: (Integer, Symbol) -> WR CFunDef {{{2
createThreadFunction :: (Integer, Symbol) -> WR CFunDef
createThreadFunction (tid, name) =
  ask >>= (\cg ->
    let
      intro = CBlockStmt (CIf (CBinary CNeqOp (CVar (ident contVar) un) (CVar (ident "null") un) un) (CGotoPtr (CVar (ident contVar) un) un) Nothing un)
      onlyDefs name = (not $ is_blocking cg name) && (is_critical cg name)
      callOrder = filter onlyDefs $ call_order cg name
      mapFunction chain = inlineCriticalFunction chain fis (last chain)
      functions = concatMap mapFunction $ List.drop 1 $ List.inits callChain
    in
      return $ CFunDef [CTypeSpec (CVoidType un)] (CDeclr (Just (ident (handlerFunction tid))) [CFunDeclr (Right ([CDecl [CTypeSpec (CVoidType un)] [(Just (CDeclr (Just (ident contVar)) [CPtrDeclr [] un] Nothing [] un), Nothing, Nothing)] un], False)) [] un] Nothing [] un) [] (CCompound [] (intro : functions) un) un
    )

-- inlineCriticalFunction :: CallChain -> FunctionInfos -> Symbol -> [CBlockItem] {{{2
type CallChain = [Symbol]
inlineCriticalFunction :: CallChain -> FunctionInfos -> Symbol -> [CBlockItem]
inlineCriticalFunction cc fis fName = lbl : body' ++ close : []
  where
    result :: (CStat, UpState)
    result = traverseCStat body $ DownState cc (fiVariables fi) fis fName 1
    fi = fis Map.! fName
    body = fromJust $ fiBody fi
    body' = extractBody $ fst result
    extractBody (CCompound _ body _) = body
    lbl = createLabel fName 0
    close = CBlockStmt $ CReturn Nothing un

data DownState = DownState {
    dCc :: CallChain
  , dSt :: SymTab
  , dFis :: FunctionInfos
  , dFName :: Symbol
  , dLbl :: Int
  }

type UpState = ()

instance DownVisitor DownState

instance UpVisitor DownState UpState where
  -- rewrite access to local variables
  upCExpr o@(CVar iden _) d _
    | Map.member name (dSt d) = (stackAccess (dCc d) (Just name), mempty)
    | otherwise = (o, mempty)
    where name = symbol iden

  upCExpr o _ _ = (o, mempty)

instance ListVisitor DownState UpState where
  -- rewrite critical function calls
  nextCBlockItem o@(CBlockStmt (CExpr (Just (CCall (CVar iden _) params _)) _)) d u
    | Map.member name (dFis d) = (criticalFunctionCall d name params Nothing, nextLabel d, u)
    | otherwise = ([o], d, u)
    where
      name = symbol iden

  nextCBlockItem o@(CBlockStmt (CExpr (Just (CAssign CAssignOp lhs (CCall (CVar fName _) params _) _)) _)) d u
    | Map.member name (dFis d) = (criticalFunctionCall d name params (Just lhs), nextLabel d, u)
    | otherwise = ([o], d, u)
    where
      name = symbol fName

  nextCBlockItem o d u = ([o], d, u)

-- criticalFunctionCall :: DownState -> Symbol -> [CExpr] -> Maybe CExpr -> [CBlockItem] {{{3
criticalFunctionCall :: DownState -> Symbol -> [CExpr] -> Maybe CExpr -> [CBlockItem]
criticalFunctionCall d cfName params resultLhs = parameters ++ continuation : call : return : lbl : result ?: []
  where
    lid = dLbl d
    chain' = (dCc d) ++ [cfName]
    fi = (dFis d) Map.! cfName

    parameters = map (createParamAssign chain') $ zip params $ fiParams fi

    continuation = CBlockStmt (CExpr (Just (CAssign CAssignOp (stackAccess chain' (Just contVar)) (CUnary CAdrOp (CVar (ident $ label (dFName d) lid) un) un) un)) un)

    call = CBlockStmt (CExpr (Just (CCall (CVar (ident cfName) un) [CUnary CAdrOp (stackAccess chain' Nothing) un] un)) un)

    return = CBlockStmt (CReturn Nothing un)

    lbl = createLabel (dFName d) lid
    result = fmap assignResult resultLhs
    assignResult lhs = createAssign lhs (stackAccess chain' (Just resVar))

-- createParamAssign :: CallChain -> (CExpr, CDecl) -> CBlockItem {{{4
createParamAssign :: CallChain -> (CExpr, CDecl) -> CBlockItem
createParamAssign chain (exp, decl) = createAssign lhs rhs
  where
    lhs = stackAccess chain $ Just $ symbol decl
    rhs = exp

createAssign :: CExpr -> CExpr -> CBlockItem
createAssign lhs rhs = CBlockStmt (CExpr (Just (CAssign CAssignOp lhs rhs un)) un)

-- stackAccess :: CallChain -> Maybe Symbol -> CExpr {{{3
stackAccess :: CallChain -> Maybe Symbol -> CExpr
stackAccess (sr:chain) variable = foldl create base $ zip pointers members
  where
    variables = if isJust variable then [fromJust variable] else []
    base = CVar (ident $ stackVar sr) un
    pointers = True : cycle [False]
    members = foldr (\x l -> frameUnion : x : l) [] chain ++ variables
    create inner (pointer, member) = CMember inner (ident member) pointer un 

-- createLabel {{{3
createLabel name id = CBlockStmt $ CLabel (ident (label name id)) (CExpr Nothing un) [] un

nextLabel d = d {dLbl = (dLbl d) + 1}
