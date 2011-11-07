{-# LANGUAGE TemplateHaskell #-}
module Ocram.Transformation.Inline.UniqueIdentifiers
-- export {{{1
(
  unique_identifiers
) where

-- import {{{1
import Control.Monad.State (evalState, State, get, put)
import Data.Generics (gmapM, mkQ, mkM, extM, GenericQ, GenericM, GenericT)
import Data.Maybe (catMaybes)
import Language.C.Syntax.AST
import Ocram.Symbols (symbol, Symbol)
import Ocram.Transformation.Inline.Types (Transformation)
import Ocram.Transformation.Util (ident, map_critical_functions)
import Ocram.Util (lookup_s, abort)
import qualified Data.Map as Map

unique_identifiers :: Transformation -- {{{1
unique_identifiers cg ast@(CTranslUnit ds _) =
  return $ map_critical_functions cg ast (transform globalIds)
  where
    globalIds = foldl newIdentifier emptyIds $ map symbol ds

    traverse :: Monad m => GenericQ Bool -> GenericM m -> GenericM m
    traverse q f x
      | q x = f x
      | otherwise = f x >>= gmapM (traverse q f)

    transform :: Identifiers -> GenericT
    transform ids x = evalState (traverse (mkQ False quit) (mkM trDecl `extM` trStat `extM` trExpr) x) ids

    quit :: CStat -> Bool
    quit (CCompound _ _ _) = True
    quit _ = False

    trDecl :: CDecl -> State Identifiers CDecl
    trDecl cd = renameDecl cd
    
    trStat :: CStat -> State Identifiers CStat
    trStat (CCompound x1 items x2) = do
      ids <- get 
      let items' = transform ids items
      return $ CCompound x1 items' x2
    
    trStat o = return o

    trExpr :: CExpr -> State Identifiers CExpr
    trExpr (CVar identifier ni) = do
      ids <- get
      let identifier' = getIdentifier ids (symbol identifier)
      return $ CVar (ident identifier') ni

    trExpr o = return o
  
type ExtraSymbols = Map.Map Symbol Int
type RenameTable = Map.Map Symbol Symbol
data Identifiers = Identifiers ExtraSymbols RenameTable deriving (Show)

emptyIds :: Identifiers
emptyIds = Identifiers Map.empty Map.empty

getIdentifier :: Identifiers -> Symbol -> Symbol
getIdentifier (Identifiers _ rt) name = $lookup_s rt name

renameDecl :: CDecl -> State Identifiers CDecl
renameDecl (CDecl x1 ds x2) = do
  ids <- get
  let ids' = foldl newIdentifier ids $ map symbol $ catMaybes $ map (\(x, _, _) -> x) ds
  let ds' = map (\(d, x, y) -> (fmap (renameDeclr ids') d, x, y)) ds
  put ids'
  return $ CDecl x1 ds' x2
  
renameDeclr :: Identifiers -> CDeclr -> CDeclr
renameDeclr ids (CDeclr (Just oldIdent) x2 x3 x4 x5) =
  let newIdent = ident $ getIdentifier ids (symbol oldIdent) in
  CDeclr (Just newIdent) x2 x3 x4 x5
renameDeclr _ _ = $abort $ "unexpected parameters"

newIdentifier :: Identifiers -> Symbol -> Identifiers
newIdentifier (Identifiers es rt) identifier = case Map.lookup identifier rt of
  Nothing -> Identifiers es (Map.insert identifier identifier rt)
  Just _ ->
    let (es', identifier') = newExtraSymbol es identifier in
    Identifiers es' (Map.insert identifier identifier' rt)

newExtraSymbol :: ExtraSymbols -> Symbol -> (ExtraSymbols, Symbol)
newExtraSymbol es name = case Map.lookup name es of
  Nothing -> (Map.insert name 0 es, mangle name 0)
  Just x -> (Map.adjust (+1) name es, mangle name (x+1))

mangle :: String -> Int -> String
mangle name index = symbol $ name ++ "_" ++ (show index)
    -- TODO avoid collisions with existing names, i.e. forbid this scheme in input code
