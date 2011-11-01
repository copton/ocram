{-# LANGUAGE TemplateHaskell #-}
module Ocram.Transformation.Inline.UniqueIdentifiers
-- export {{{1
(
  unique_identifiers
) where

-- import {{{1
import Control.Monad.Reader (ask)
import Data.Maybe (catMaybes)
import Data.Monoid (mempty)
import Language.C.Syntax.AST
import Ocram.Query (function_parameters_fd)
import Ocram.Symbols (symbol, Symbol)
import Ocram.Transformation.Util (ident, map_critical_functions)
import Ocram.Transformation.Inline.Types (WR)
import Ocram.Types (Ast)
import Ocram.Util (lookup_s, abort)
import Ocram.Visitor (DownVisitor(..), UpVisitor(..), ListVisitor, traverseCFunDef)
import qualified Data.Map as Map

unique_identifiers :: Ast -> WR Ast -- {{{1
unique_identifiers ast@(CTranslUnit ds _) = do
  cg <- ask
  let ids = Identifiers Map.empty Map.empty
  let ids' = foldl newIdentifier ids $ map getSymbol ds
  let f = flip traverseCFunDef ids' :: (CFunDef -> (CFunDef, UpState))
  return $ map_critical_functions cg ast $ fst . f
  where
    getSymbol (CDeclExt cd) = symbol cd
    getSymbol (CFDefExt fd) = symbol fd
    getSymbol _ = $abort "unexpected parameters"

type ExtraSymbols = Map.Map Symbol Int
type RenameTable = Map.Map Symbol Symbol

data Identifiers = Identifiers ExtraSymbols RenameTable deriving (Show)

newExtraSymbol :: ExtraSymbols -> Symbol -> (ExtraSymbols, Symbol)
newExtraSymbol es name = case Map.lookup name es of
  Nothing -> (Map.insert name 0 es, mangle name 0)
  Just x -> (Map.adjust (+1) name es, mangle name (x+1))

mangle :: String -> Int -> String
mangle name index = symbol $ name ++ "_" ++ (show index)
-- TODO avoid collisions with existing names, i.e. forbid this scheme in input code

newIdentifier :: Identifiers -> Symbol -> Identifiers
newIdentifier (Identifiers es rt) identifier = case Map.lookup identifier rt of
  Nothing -> Identifiers es (Map.insert identifier identifier rt)
  Just _ ->
    let (es', identifier') = newExtraSymbol es identifier in
    Identifiers es' (Map.insert identifier identifier' rt)

getIdentifier :: Identifiers -> Symbol -> Symbol
getIdentifier (Identifiers _ rt) name = $lookup_s rt name

addDecls :: Identifiers -> CDecl -> Identifiers
addDecls ids (CDecl _ ds _) = 
  foldl newIdentifier ids $ map symbol $ catMaybes $ map (\(x, _, _) -> x) ds

type UpState = ()

instance DownVisitor Identifiers where
  downCFunDef fd ids =
    foldl newIdentifier ids $ map symbol $ function_parameters_fd fd

  downCBlockItem (CBlockDecl cd) ids = addDecls ids cd

  downCBlockItem _ d = d

  downCStat (CFor (Right cd) _ _ _ _) ids = addDecls ids cd

  downCStat _ d = d

instance UpVisitor Identifiers UpState where
  upCBlockItem (CBlockDecl cd) ids _ = (CBlockDecl (renameDecl ids cd), mempty)

  upCBlockItem o _ u = (o, u)

  upCStat (CFor (Right cd) x1 x2 x3 x4) ids _ =
    (CFor (Right (renameDecl ids cd)) x1 x2 x3 x4, mempty)

  upCStat o _ u = (o, u)

  upCExpr (CVar identifier ni) ids _ =
    (CVar (ident (getIdentifier ids (symbol identifier))) ni, mempty)
  
  upCExpr o _ u = (o, u)

instance ListVisitor Identifiers UpState

renameDecl :: Identifiers -> CDecl -> CDecl
renameDecl ids (CDecl x1 ds x2) = CDecl x1 ds' x2
  where
    ds' = map (\(d, x, y) -> (fmap (renameDeclr ids) d, x, y)) ds

renameDeclr :: Identifiers -> CDeclr -> CDeclr
renameDeclr ids (CDeclr (Just oldIdent) x2 x3 x4 x5) =
  let newIdent = ident $ getIdentifier ids (symbol oldIdent) in
  CDeclr (Just newIdent) x2 x3 x4 x5
renameDeclr _ _ = $abort $ "unexpected parameters"
