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
import Data.List (partition)
import Data.Maybe (mapMaybe, fromMaybe)
import Language.C.Data.Ident (internalIdent)
import Language.C.Data.Node (NodeInfo, undefNode)
import Language.C.Syntax.AST
import Ocram.Intermediate.Representation
import Ocram.Names (varShadow, varStatic)
import Ocram.Query (function_parameters_fd)
import Ocram.Symbols (symbol, Symbol)
import Ocram.Util (abort, unexp)

import qualified Data.Map as M

collect_declarations :: CFunDef -> ([CBlockItem], [Variable], [Variable]) -- {{{1
collect_declarations fd@(CFunDef _ _ _ (CCompound _ items funScope) _) =
  (body, autoVars, staticVars)
  where
    ps = function_parameters_fd fd
    initialIdents = Identifiers (M.fromList (map initCnt ps)) (M.fromList (map initRen ps))
    initCnt x = (symbol x, 0)
    initRen x = (symbol x, symbol x)
    initialState  = Ctx initialIdents funScope [] [] (symbol fd)
    (statements, state) = runState (mapM mItem items) initialState

    params = map (\cd -> Variable cd (symbol cd) (Just funScope)) ps

    body = concat statements
    autoVars = params ++ ctxAutoVars state
    staticVars = ctxStaticVars state

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
  (Ctx curIds curScope curAutoVars curStaticVars fun) <- get
  let (statements, Ctx newIds _ newAutoVars newStaticVars _) = runState (mapM mItem items') (Ctx curIds innerScope curAutoVars curStaticVars fun)
  put $ Ctx (Identifiers (idCnt newIds) (idRen curIds)) curScope newAutoVars newStaticVars fun
  return $ CCompound x (concat statements) innerScope

tStmt (CFor (Right decl) x1 x2 body ni) = do
  -- the scope of the declared variable is the body of the for loop
  (Ctx ids scope autoVars staticVars fun) <- get 
  let (Ctx ids' _ autoVars' staticVars' _, inits) = trDecl' (Ctx ids (annotation body) autoVars staticVars fun) decl 
  put (Ctx ids' scope autoVars' staticVars' fun)

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
trDecl' (Ctx ids scope autoVars staticVars fun) decl = 
  let
    decls = unlistDecl decl
    (staticDecls, autoDecls) = partition isStatic decls

    ids' = foldl (addIdentifier id) ids $ map symbol autoDecls
    ids'' = foldl (addIdentifier (varStatic fun)) ids' $ map symbol staticDecls

    newAutoNames   = map (getIdentifier ids'' . symbol) autoDecls
    newStaticNames = map (getIdentifier ids'' . symbol) staticDecls

    (autoDeclsOnly, autoInitExpr) = unzip $ map split autoDecls

    autoVars'      = map mkVar $ zip autoDeclsOnly newAutoNames
    staticVars'    = map mkVar $ zip staticDecls newStaticNames

    autoInitStmt   = mapMaybe (uncurry mkInit) $ zip autoInitExpr newAutoNames

    ctx = Ctx ids'' scope (autoVars' ++ autoVars) (staticVars' ++ staticVars) fun
  in (ctx, autoInitStmt)
  where
    unlistDecl (CDecl x [] ni) = [CDecl x [] ni] -- struct S { int i; };
    unlistDecl (CDecl s ds ni) = map (\x -> CDecl s [x] ni) ds

    split (CDecl y1 [(Just declr, Just (CInitExpr cexpr _), y2)] y3) =
      let declare = (CDecl y1 [(Just declr, Nothing, y2)] y3) in
      (declare, Just cexpr)
    split cdecl = (cdecl, Nothing)

    isStatic (CDecl ds _ _) = any isStaticSpec ds
    isStaticSpec (CStorageSpec (CStatic _)) = True
    isStaticSpec _                          = False

    mkVar (cd, name) = Variable cd name (Just scope)

    mkInit Nothing    _    =  Nothing
    mkInit (Just rhs) name =  Just $ CExpr (Just (CAssign CAssignOp (CVar (internalIdent name) ni) rhs ni)) ni
      where ni = annotation rhs

-- types {{{1
data Ctx = Ctx { -- {{{2
    ctxIdents     :: Identifiers
  , ctxScope      :: NodeInfo
  , ctxAutoVars   :: [Variable]
  , ctxStaticVars :: [Variable]
  , ctxFun        :: Symbol
  }

data Identifiers = Identifiers { -- {{{2
    idCnt :: M.Map Symbol Int    -- ^counting the occurences of each identifier
  , idRen :: M.Map Symbol Symbol -- ^the current renaming table
  } deriving (Show)

type ExtraSymbols = M.Map Symbol Int -- {{{2

type RenameTable = M.Map Symbol Symbol -- {{{2

type S a = State Ctx a -- {{{2

-- utils {{{1
getIdentifier :: Identifiers -> Symbol -> Symbol -- {{{2
getIdentifier (Identifiers _ rt) name = fromMaybe name $ M.lookup name rt

addIdentifier :: (String -> String) -> Identifiers -> Symbol -> Identifiers -- {{{2
addIdentifier mangle (Identifiers es rt) identifier = case M.lookup identifier es of
  Nothing -> Identifiers (M.insert identifier 0 es) (M.insert identifier (mangle identifier) rt)
  Just count -> Identifiers (M.adjust (+1) identifier es) (M.insert identifier (mangle (varShadow identifier count)) rt)
