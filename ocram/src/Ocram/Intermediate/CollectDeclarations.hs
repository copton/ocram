{-# LANGUAGE TemplateHaskell, RankNTypes #-}
module Ocram.Intermediate.CollectDeclarations 
-- exports {{{1
(
  collect_declarations
) where

-- imports {{{1
import Control.Applicative ((<$>), (<*>))
import Control.Monad (zipWithM)
import Control.Monad.State (State, get, gets, put, runState, gets, modify)
import Data.Data (Data, gmapM)
import Data.Generics (everywhereM, mkM, extM, mkQ, GenericQ, GenericM)
import Data.List (partition)
import Data.Maybe (fromMaybe, catMaybes)
import Language.C.Data.Ident (internalIdent)
import Language.C.Data.Node (NodeInfo, undefNode, posOfNode, getLastTokenPos)
import Language.C.Data.Position (posRow)
import Language.C.Syntax.AST
import Ocram.Intermediate.Representation
import Ocram.Names (varUnique, varStatic)
import Ocram.Query (function_parameters_fd, object_type)
import Ocram.Ruab (TRow(..))
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

    params = map (\cd -> TVariable (symbol cd) cd (scopeOfNode funScope)) ps

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
  --  the scope of the declared variable is the body of the for loop
  scope <- gets ctxScope
  modify (\ctx -> ctx {ctxScope = (annotation body)})
  inits <- trDecl decl
  modify (\ctx -> ctx {ctxScope = scope})
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
  (Ctx ids scope autoVars staticVars fun) <- get
  let
    decls = unlistDecl decl
    (staticDecls, autoDecls) = partition isStatic decls

    ids' = foldl (addIdentifier id) ids $ map symbol autoDecls
    ids'' = foldl (addIdentifier (varStatic fun)) ids' $ map symbol staticDecls

    newAutoNames   = map (getIdentifier ids'' . symbol) autoDecls
    newStaticNames = map (getIdentifier ids'' . symbol) staticDecls

    (autoDeclsOnly, autoInits) = unzip $ map split autoDecls

    autoVars'      = zipWith (mkVar scope) autoDeclsOnly newAutoNames
    staticVars'    = zipWith (mkVar scope) (map rmStatic staticDecls) newStaticNames

  autoInitStmt   <- zipWithM mkInit autoInits newAutoNames
  put $ Ctx ids'' scope (autoVars' ++ autoVars) (staticVars' ++ staticVars) fun
  return $ catMaybes autoInitStmt

  where
    unlistDecl (CDecl x [] ni) = [CDecl x [] ni] -- struct S { int i; };
    unlistDecl (CDecl s ds ni) = map (\x -> CDecl s [x] ni) ds

    split :: CDeclaration a -> (CDeclaration a, Maybe (CInitializer a, CTypeSpecifier a))
    split cd@(CDecl y1 [(Just declr, Just cinit, y2)] y3) =
      let declare = (CDecl y1 [(Just declr, Nothing, y2)] y3) in
      (declare, Just (cinit, object_type cd))
    split cdecl = (cdecl, Nothing)

    isStatic (CDecl ds _ _) = any isStaticSpec ds
    isStaticSpec (CStorageSpec (CStatic _)) = True
    isStaticSpec _                          = False

    rmStatic (CDecl ds x1 x2) = CDecl (filter (not . isStaticSpec) ds) x1 x2

    mkVar scope cd name = TVariable (symbol cd) (renameDecl name cd) (scopeOfNode scope)

    mkInit Nothing                          _    = return Nothing
    mkInit (Just (initializer, objectType)) name = case initializer of
      CInitExpr expr  _ -> newInit expr
      CInitList initl _ -> 
        let
          cd = CDecl [CTypeSpec objectType] [] (annotation initializer)
          cl = CCompoundLit cd initl (annotation initializer)
        in newInit cl
      where
        newInit rhs = do
          rhs' <- everywhereM (mkM tExpr) rhs
          return $ Just $ CExpr (Just (CAssign CAssignOp (CVar (internalIdent name) ni) rhs' ni)) ni
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
  Just count -> Identifiers (M.adjust (+1) identifier es) (M.insert identifier (mangle (varUnique identifier count)) rt)

renameDecl :: Symbol -> CDecl -> CDecl -- {{{2
renameDecl newName (CDecl x1 [(Just declr, x2, x3)] x4) =
  CDecl x1 [(Just (renameDeclr newName declr), x2, x3)] x4
  where
    renameDeclr newName' (CDeclr (Just _) y1 y2 y3 y4) =
      CDeclr (Just (internalIdent newName')) y1 y2 y3 y4
    renameDeclr _ x = $abort $ unexp x

renameDecl _ x = $abort $ unexp x

scopeOfNode :: NodeInfo -> (TRow, TRow)
scopeOfNode = (,) <$> TRow . posRow . posOfNode <*> TRow . posRow . fst . getLastTokenPos
