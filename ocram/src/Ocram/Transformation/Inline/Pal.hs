module Ocram.Transformation.Inline.Pal
-- export {{{1
(
  extract_pal, compare_pal
) where

-- import {{{1
import Data.Generics (everything, mkQ)
import Data.Maybe (mapMaybe)
import Language.C.Syntax.AST
import Ocram.Analysis (CallGraph, blocking_functions)
import Ocram.Print (pretty, ENodeInfo)
import Ocram.Symbols (symbol)
import Ocram.Transformation.Inline.Names (frameType) 
import Ocram.Transformation.Util (un)
import Ocram.Text (new_error, OcramError)
import qualified Data.Map as Map
import qualified Data.Set as Set

extract_pal :: CallGraph -> CTranslationUnit ENodeInfo -> CTranslationUnit ENodeInfo -- {{{1
extract_pal cg ast = CTranslUnit (map CDeclExt (extractPal cg ast)) un

compare_pal :: CallGraph -> CTranslationUnit ENodeInfo -> CTranslationUnit ENodeInfo -> Either [OcramError] () -- {{{1
compare_pal cg source target =
  if null errors
    then Right ()
    else Left errors
  where
  errors = mapMaybe check $ Map.toList source'
  check (k, v) = case Map.lookup k target' of
    Nothing -> Just $ new_error 1 ("PAL declaration for '" ++ k ++ "' is missing. Should be: '" ++ v ++ "'.") Nothing
    Just v' -> if v /= v'
      then Just $ new_error 2 ("PAL declaration for '" ++ k ++ "' differs. Should be: '" ++ v ++ "')") Nothing
      else Nothing
  source' = flatten source
  target' = flatten target
  flatten ast = foldr (\cd m -> Map.insert (symbol cd) (show (pretty cd)) m) Map.empty (extractPal cg ast)

extractPal :: CallGraph -> CTranslationUnit ENodeInfo -> [CDeclaration ENodeInfo] -- {{{2
extractPal cg (CTranslUnit ds _) = ds'
  where
  bf = Set.fromList $ blocking_functions cg
  frames = Set.map frameType bf
  ds' = everything (++) (mkQ [] query) ds
  query (CDeclExt cd@(CDecl _ _ _))
    | (symbol cd) `Set.member` bf = [cd]
    | symbol cd `Set.member` frames = [cd]
    | otherwise = []
  query _ = []
