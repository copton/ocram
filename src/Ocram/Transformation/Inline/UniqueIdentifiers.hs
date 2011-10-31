{-# LANGUAGE TemplateHaskell #-}
module Ocram.Transformation.Inline.UniqueIdentifiers
-- export {{{1
(
  unique_identifiers
) where

-- import {{{1
import Control.Monad.Reader (ask)
import Data.Monoid (mempty)
import Language.C.Syntax.AST
import Language.C.Pretty (pretty)
import Ocram.Analysis (is_critical, CallGraph)
import Ocram.Query (function_parameters)
import Ocram.Symbols (symbol, Symbol)
import Ocram.Transformation.Util (ident)
import Ocram.Transformation.Inline.Types (WR)
import Ocram.Types (Ast)
import Ocram.Util (lookup_s, abort)
import Ocram.Visitor (DownVisitor(..), UpVisitor(..), ListVisitor, traverseCTranslUnit, traverseCFunDef)
import qualified Data.Map as Map

unique_identifiers :: Ast -> WR Ast -- {{{1
unique_identifiers ast = do
  cg <- ask
  return $ fst $ (traverseCTranslUnit ast $ DownState cg (Identifiers Map.empty Map.empty) :: (CTranslUnit, UpState))

type ExtraSymbols = Map.Map Symbol Int
type RenameTable = Map.Map Symbol Symbol

data Identifiers = Identifiers ExtraSymbols RenameTable

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

data DownState = DownState {
  dCg :: CallGraph,
  dIds :: Identifiers
  }

type UpState = ()

instance DownVisitor DownState where
  downCExtDecl (CDeclExt cd) d = d {dIds = newIdentifier (dIds d) (symbol cd)}
  downCExtDecl (CFDefExt fd) d = d {dIds = newIdentifier (dIds d) (symbol fd)}
  downCExtDecl _ _ = $abort "unexpected parameters"

instance UpVisitor DownState UpState where
  upCFunDef fd d _
    | is_critical (dCg d) (symbol fd) = (fst travRes, mempty)
    | otherwise = (fd, mempty)
      where
        travRes :: (CFunDef, UpState)
        travRes = traverseCFunDef fd (dIds d)

instance ListVisitor DownState UpState


instance DownVisitor Identifiers where
  downCFunDef fd ids =
    foldl newIdentifier ids $ map symbol $ function_parameters fd

  downCBlockItem (CBlockDecl cd) ids =
    newIdentifier ids (symbol cd)

  downCBlockItem _ d = d

  downCStat (CFor (Right cd) _ _ _ _) ids =
    newIdentifier ids (symbol cd)

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
renameDecl ids (CDecl x1 [(Just (CDeclr (Just oldIdent) x2 x3 x4 x5), x6, x7)] x8) =
  let newIdent = ident $ getIdentifier ids (symbol oldIdent) in
  CDecl x1 [(Just (CDeclr (Just newIdent) x2 x3 x4 x5), x6, x7)] x8
renameDecl _ cd = $abort $ "unexpected parameters: " ++ show (pretty cd) 
