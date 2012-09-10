{-# LANGUAGE TemplateHaskell, RankNTypes #-}
module Ocram.Intermediate.CollectDeclarations 
-- exports {{{1
(
  collect_declarations
) where

-- imports {{{1
import Control.Monad.State (State, get, put, runState, gets)
import Data.Data (Data, gmapM)
import Data.Generics (mkM, extM, mkQ, GenericQ, GenericM)
import Data.Maybe (fromMaybe, mapMaybe)
import Language.C.Data.Ident (internalIdent)
import Language.C.Data.Node (NodeInfo, undefNode)
import Language.C.Syntax.AST
import Ocram.Intermediate.Representation
import Ocram.Names (varShadow)
import Ocram.Query (function_parameters_fd)
import Ocram.Symbols (symbol, Symbol)
import Ocram.Util (abort, unexp)

import qualified Data.Map as M

collect_declarations :: CFunDef -> ([Variable], [CBlockItem]) -- {{{1
collect_declarations fd@(CFunDef _ _ _ (CCompound _ items funScope) _) =
  (variables, body)
  where
    ps = function_parameters_fd fd
    initialIdents = Identifiers M.empty (M.fromList (map ((\x -> (x, x)) . symbol) ps))
    initialState  = Ctx initialIdents funScope []
    (statements, state) = runState (mapM mItem items) initialState

    params = map (\cd -> Variable cd (symbol cd) funScope) ps

    body = concat statements
    variables = params ++ ctxVars state

collect_declarations x = $abort $ unexp x

mItem :: CBlockItem -> S [CBlockItem] -- {{{2
mItem (CBlockStmt stmt) = do
  stmt' <- topDown (mkQ False qStmt) (mkM tExpr `extM` tStmt) stmt
  return [CBlockStmt stmt']

mItem (CBlockDecl decl) = trDecl decl >>= return . map CBlockStmt

mItem x                 = $abort $ unexp x

topDown :: (Monad m, Data a) => GenericQ Bool -> GenericM m -> a -> m a -- {{{2
topDown q f x
  | q x == True = f x
  | otherwise = f x >>= gmapM (topDown q f)

qStmt :: CStat -> Bool
qStmt (CCompound _ _ _) = True
qStmt _                 = False

tStmt :: CStat -> S CStat -- {{{2

tStmt (CCompound x items' innerScope) = do
  (Ctx curIds curScope curVars) <- get
  let (statements, Ctx newIds _ newVars) = runState (mapM mItem items') (Ctx curIds innerScope curVars)
  put $ Ctx (curIds {idEs = idEs newIds}) curScope newVars
  return $ CCompound x (concat statements) innerScope

tStmt (CFor (Right decl) x1 x2 body ni) = do
  -- the scope of the declared variable is the body of the for loop
  (Ctx ids scope vars) <- get 
  let (Ctx ids' _ vars', inits) = trDecl' (Ctx ids (annotation body) vars) decl 
  put (Ctx ids' scope vars')

  let for   = CFor (Left Nothing) x1 x2 body ni
  let block = map CBlockStmt $ inits ++ [for]
  return $ CCompound [] block undefNode

tStmt o  = return o

tExpr :: CExpr -> S CExpr -- {{{2
tExpr (CVar ident ni) = do
  ids <- gets ctxIdents
  let name = getIdentifier ids (symbol ident)
  return $ CVar (internalIdent name) ni

tExpr o = return o

trDecl :: CDecl -> S [CStat] -- {{{2
trDecl decl = do
  ctx <- get
  let (ctx', inits) = trDecl' ctx decl
  put ctx'
  return inits

trDecl' :: Ctx -> CDecl -> (Ctx, [CStat])
trDecl' (Ctx ids scope vars) decl = 
  let
    (decls, rhss) = (unzip . map split . unlistDecl) decl 
    ids' = foldl addIdentifier ids $ map symbol decls
    newNames = map (getIdentifier ids' . symbol) decls
    vars' = map (\(cd, name) -> Variable cd name scope) $ zip decls newNames
    inits = mapMaybe (uncurry mkInit) $ zip rhss newNames
    ctx = Ctx ids' scope (vars' ++ vars)
  in (ctx, inits)
  where
    unlistDecl (CDecl x [] ni) = [CDecl x [] ni] -- struct S { int i; };
    unlistDecl (CDecl s ds ni) = map (\x -> CDecl s [x] ni) ds

    split (CDecl y1 [(Just declr, Just (CInitExpr cexpr _), y2)] y3) =
      let declare = (CDecl y1 [(Just declr, Nothing, y2)] y3) in
      (declare, Just cexpr)
    split cdecl = (cdecl, Nothing)

    mkInit Nothing    _    =  Nothing
    mkInit (Just rhs) name =  Just $ CExpr (Just (CAssign CAssignOp (CVar (internalIdent name) ni) rhs ni)) ni
      where ni = annotation rhs

-- types {{{1
data Ctx = Ctx { -- {{{2
    ctxIdents :: Identifiers
  , ctxScope  :: NodeInfo
  , ctxVars   :: [Variable]
  }

data Identifiers = Identifiers { -- {{{2
  idEs :: ExtraSymbols,
  idRt :: RenameTable
  } deriving (Show)

type ExtraSymbols = M.Map Symbol Int -- {{{2

type RenameTable = M.Map Symbol Symbol -- {{{2

type S a = State Ctx a -- {{{2

-- utils {{{1
getIdentifier :: Identifiers -> Symbol -> Symbol -- {{{2
getIdentifier (Identifiers _ rt) name = fromMaybe name (M.lookup name rt)

addIdentifier :: Identifiers -> Symbol -> Identifiers -- {{{2
addIdentifier (Identifiers es rt) identifier = case M.lookup identifier rt of
  Nothing -> Identifiers es (M.insert identifier identifier rt)
  Just _ ->
    let (es', identifier') = newExtraSymbol in
    Identifiers es' (M.insert identifier identifier' rt)
  where
    newExtraSymbol = case M.lookup identifier es of
      Nothing -> (M.insert identifier 0 es, varShadow identifier  0)
      Just x -> (M.adjust (+1) identifier es, varShadow identifier (x+1))
