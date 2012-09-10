{-# LANGUAGE TemplateHaskell #-}
module Ocram.Intermediate.CollectDeclarations 
-- exports {{{1
(
  collect_declarations
) where

-- imports {{{1
import Control.Monad.State (State, get, put, runState, gets)
import Data.Generics (everywhereM, mkM, extM)
import Data.Maybe (fromMaybe, catMaybes)
import Language.C.Syntax.AST
import Language.C.Data.Ident (internalIdent)
import Language.C.Data.Node (NodeInfo, undefNode)
import Ocram.Intermediate.Representation
import Ocram.Symbols (symbol, Symbol)
import Ocram.Util (abort, unexp)
import Ocram.Query (function_parameters_fd)
import Ocram.Transformation.Names (varShadow) -- XXX: bad dependency

import qualified Data.Map as M

collect_declarations :: CFunDef -> ([Variable], [CBlockItem]) -- {{{1
collect_declarations fd@(CFunDef _ _ _ (CCompound _ items funScope) _) =
  (variables, body)
  where
    ps = function_parameters_fd fd
    initialIdents = Identifiers M.empty (M.fromList (map ((\x -> (x, x)) . symbol) ps))
    initialState  = Ctx initialIdents funScope []
    (statements, state) = runState (mapM trItem items) initialState

    params = map (\cd -> Variable cd (symbol cd) funScope) ps

    body = concat statements
    variables = params ++ ctxVars state

collect_declarations x = $abort $ unexp x

trItem :: CBlockItem -> S [CBlockItem] -- {{{2
trItem (CBlockStmt stmt) = everywhereM (mkM trStmt `extM` trExpr) stmt >>= return . (:[]) . CBlockStmt
trItem (CBlockDecl decl) = trDecl decl >>= return . map CBlockStmt
trItem x                 = $abort $ unexp x

trStmt :: CStat -> S CStat -- {{{2

trStmt (CCompound x items' innerScope) = do
  (Ctx curIds curScope curVars) <- get
  let (statements, Ctx newIds _ newVars) = runState (mapM trItem items') (Ctx curIds innerScope curVars)
  put $ Ctx (curIds {idEs = idEs newIds}) curScope newVars
  return $ CCompound x (concat statements) innerScope

trStmt (CFor (Right decl) x2 x3 s x4) = do
  inits <- trDecl decl
  let for   = CFor (Left Nothing) x2 x3 s x4
  let block = map CBlockStmt $ inits ++ [for]
  return $ CCompound [] block undefNode

trStmt o  = return o

trExpr :: CExpr -> S CExpr -- {{{2
trExpr (CVar ident ni) = do
  ids <- gets ctxIdents
  let name = getIdentifier ids (symbol ident)
  return $ CVar (internalIdent name) ni

trExpr o = return o

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
trDecl :: CDecl -> S [CStat] -- {{{2
trDecl decl = do
  (Ctx ids scope vars) <- get
  let
    (decls, rhss) = (unzip . map split . unlistDecl) decl 
    ids' = foldl addIdentifier ids $ map symbol decls
    newNames = map (getIdentifier ids' . symbol) decls
    vars' = map (\(cd, name) -> Variable cd name scope) $ zip decls newNames
    inits = map (uncurry mkInit) $ zip rhss newNames
  put $ Ctx ids' scope (vars' ++ vars)
  return $ catMaybes inits
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
