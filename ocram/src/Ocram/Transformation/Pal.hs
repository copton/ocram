module Ocram.Transformation.Pal
-- export {{{1
(
  extract_pal
) where

-- import {{{1
import Data.Generics (everything, mkQ)
import Language.C.Syntax.AST
import Ocram.Analysis (CallGraph, blocking_functions)
import Ocram.Debug (ENodeInfo)
import Ocram.Symbols (symbol)
import Ocram.Transformation.Names (frameType) 
import Ocram.Transformation.Util (un)
import qualified Data.Set as Set

extract_pal :: CallGraph -> CTranslationUnit ENodeInfo -> CTranslationUnit ENodeInfo -- {{{1
extract_pal cg (CTranslUnit ds _) = CTranslUnit (map CDeclExt ds') un
  where
  bf = Set.fromList $ blocking_functions cg
  frames = Set.map frameType bf
  ds' = everything (++) (mkQ [] query) ds
  query (CDeclExt cd@(CDecl _ _ _))
    | (symbol cd) `Set.member` bf = [cd]
    | symbol cd `Set.member` frames = [cd]
    | otherwise = []
  query _ = []
