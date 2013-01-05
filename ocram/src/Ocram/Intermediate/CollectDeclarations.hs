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
import Ocram.Names (varUnique)
import Ocram.Query (function_parameters_fd, object_type, rename_decl)
import Ocram.Ruab (TRow(..))
import Ocram.Symbols (symbol, Symbol)
import Ocram.Util (abort, unexp)

import qualified Data.Map as M
import qualified Data.Set as S

collect_declarations :: S.Set Symbol -> CFunDef -> ([CBlockItem], [FunctionVariable]) -- {{{1
collect_declarations sf fd@(CFunDef _ _ _ (CCompound _ items funScope) _) =
  (body, vars)
  where
    sf'           = S.elems sf
    initialIdents = Identifiers (M.fromList (map (\x -> (x, 0)) sf')) (M.fromList (map (\x -> (x, x)) sf'))
    pdecls        = function_parameters_fd fd 
    idents        = foldl addIdentifier initialIdents (map symbol pdecls)
    initialState  = Ctx idents funScope [] (symbol fd)
    (statements, state) = runState (mapM mItem items) initialState

    params        = map createParamVar pdecls
    vars          = ctxVars state ++ params
    body          = concat statements

    createParamVar decl =
      let
        oldName = symbol decl
        newName = getIdentifier idents oldName
        newDecl = rename_decl newName decl
      in
        fvarParameter $ TVariable oldName newDecl (scopeOfNode funScope)

collect_declarations _ x = $abort $ unexp x

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
  (Ctx curIds curScope curVars fun) <- get
  let (statements, Ctx newIds _ newVars _) = runState (mapM mItem items') (Ctx curIds innerScope curVars fun)
  put $ Ctx (Identifiers (idCnt newIds) (idRen curIds)) curScope newVars fun
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
  (Ctx ids scope vars fun) <- get
  let
    decls = unlistDecl decl
    (staticDecls, autoDecls) = partition isStatic decls

    ids'  = foldl addIdentifier ids  $ map symbol autoDecls
    ids'' = foldl addIdentifier ids' $ map symbol staticDecls

    newAutoNames   = map (getIdentifier ids'' . symbol) autoDecls
    newStaticNames = map (getIdentifier ids'' . symbol) staticDecls

    (autoDeclsOnly, autoInits) = unzip $ map split autoDecls

    autoVars       = map fvarAuto   $ zipWith (mkVar scope) autoDeclsOnly newAutoNames
    staticVars     = map fvarStatic $ zipWith (mkVar scope) staticDecls newStaticNames

  autoInitStmt   <- zipWithM mkInit autoInits newAutoNames
  put $ Ctx ids'' scope (vars ++ autoVars ++ staticVars) fun
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

    mkVar scope cd name = TVariable (symbol cd) (rename_decl name cd) (scopeOfNode scope)

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
  , ctxVars       :: [FunctionVariable]
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

addIdentifier :: Identifiers -> Symbol -> Identifiers -- {{{2
addIdentifier (Identifiers es rt) identifier = case M.lookup identifier es of
  Nothing -> Identifiers (M.insert identifier 0 es) (M.insert identifier identifier rt)
  Just count -> Identifiers (M.adjust (+1) identifier es) (M.insert identifier (varUnique identifier count) rt)

scopeOfNode :: NodeInfo -> (TRow, TRow)
scopeOfNode = (,) <$> TRow . posRow . posOfNode <*> TRow . posRow . fst . getLastTokenPos
