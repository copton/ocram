{-# LANGUAGE Rank2Types, TemplateHaskell #-}
module Ocram.Transformation.Normalize.UniqueIdentifiers
-- export {{{1
(
  unique_identifiers
) where

-- import {{{1
import Control.Monad.State (runState, State, get, put)
import Data.Generics (gmapM, mkQ, mkM, extM, extQ, GenericQ, GenericM, Data)
import Data.Maybe (mapMaybe, fromMaybe)
import Language.C.Syntax.AST
import Ocram.Debug (ENodeInfo)
import Ocram.Symbols (symbol, Symbol)
import Ocram.Transformation.Util (ident)
import Ocram.Transformation.Names (varShadow)
import Ocram.Transformation.Types
import Ocram.Util (abort, unexp)
import qualified Data.Map as Map

unique_identifiers :: CFunDef' -> CFunDef'
unique_identifiers = fst . transform noIds
  where
--    globalIds = Identifiers Map.empty (foldl (\m x -> Map.insert x x m) Map.empty $ map symbol ds)
    noIds = Identifiers Map.empty Map.empty

    traverse :: Monad m => GenericQ Bool -> GenericM m -> GenericM m
    traverse q f x
      | q x = f x
      | otherwise = f x >>= gmapM (traverse q f)

    transform :: Data a => Identifiers -> a -> (a, Identifiers)
    transform ids x = runState (traverse (mkQ False quitStat `extQ` quitExpr) (mkM trDecl `extM` trStat `extM` trExpr) x) ids

    quitStat :: CStatement ENodeInfo -> Bool
    quitStat (CCompound _ _ _) = True
    quitStat _ = False

    quitExpr :: CExpression ENodeInfo -> Bool
    quitExpr (CCast _ _ _) = True
    quitExpr  (CSizeofType _ _) = True
    quitExpr  (CAlignofType _ _) = True
    quitExpr  _ = False

    trDecl :: CDeclaration ENodeInfo -> State Identifiers (CDeclaration ENodeInfo)
    trDecl = renameDecl
    
    trStat :: CStatement ENodeInfo -> State Identifiers (CStatement ENodeInfo)
    trStat (CCompound x1 items x2) = do
      ids <- get 
      let (items', ids') = transform ids items
      put (ids {idEs = idEs ids'} )
      return $ CCompound x1 items' x2
    
    trStat o = return o

    trExpr :: CExpression ENodeInfo -> State Identifiers (CExpression ENodeInfo)
    trExpr (CVar identifier ni) = do
      ids <- get
      let identifier' = getIdentifier ids (symbol identifier)
      return $ CVar (ident identifier') ni

    trExpr o = return o
  
type ExtraSymbols = Map.Map Symbol Int
type RenameTable = Map.Map Symbol Symbol
data Identifiers = Identifiers {
  idEs :: ExtraSymbols,
  idRt :: RenameTable
  } deriving (Show)

emptyIds :: Identifiers
emptyIds = Identifiers Map.empty Map.empty

getIdentifier :: Identifiers -> Symbol -> Symbol
getIdentifier (Identifiers _ rt) name = fromMaybe name (Map.lookup name rt)

renameDecl :: CDeclaration ENodeInfo -> State Identifiers (CDeclaration ENodeInfo)
renameDecl (CDecl x1 ds x2) = do
  ids <- get
  let ids' = foldl newIdentifier ids $ map symbol $ mapMaybe (\(x, _, _) -> x) ds
  let ds' = map (\(d, x, y) -> (fmap (renameDeclr ids') d, x, y)) ds
  put ids'
  return $ CDecl x1 ds' x2
  
renameDeclr :: Identifiers -> CDeclarator ENodeInfo -> CDeclarator ENodeInfo
renameDeclr ids (CDeclr (Just oldIdent) x2 x3 x4 x5) =
  let newIdent = ident $ getIdentifier ids (symbol oldIdent) in
  CDeclr (Just newIdent) x2 x3 x4 x5
renameDeclr _ x = $abort $ unexp x

newIdentifier :: Identifiers -> Symbol -> Identifiers
newIdentifier (Identifiers es rt) identifier = case Map.lookup identifier rt of
  Nothing -> Identifiers es (Map.insert identifier identifier rt)
  Just _ ->
    let (es', identifier') = newExtraSymbol es identifier in
    Identifiers es' (Map.insert identifier identifier' rt)

newExtraSymbol :: ExtraSymbols -> Symbol -> (ExtraSymbols, Symbol)
newExtraSymbol es name = case Map.lookup name es of
  Nothing -> (Map.insert name 0 es, varShadow name 0)
  Just x -> (Map.adjust (+1) name es, varShadow name (x+1))
