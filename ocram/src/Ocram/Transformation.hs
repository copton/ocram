module Ocram.Transformation
-- exports {{{1
(
  transformation
) where

-- imports {{{1
import Data.Generics (everything, everywhere, mkT, mkQ, extT)
import Language.C.Syntax.AST
import Ocram.Analysis (CallGraph, blocking_functions)
import Ocram.Debug (enrich_node_info, ENodeInfo(..), un)
import Ocram.Ruab (VarMap)
import Ocram.Symbols (symbol)
import Ocram.Transformation.Normalize (normalize)
import Ocram.Transformation.Translate (translate)
import Ocram.Transformation.Types
import Ocram.Transformation.Names (frameType)

import qualified Data.Set as Set

transformation :: CallGraph -> CTranslUnit -> (CTranslUnit', CTranslUnit', VarMap) -- {{{1
transformation cg ast =
  let
    ast' = enableBreakpoints . translate cg . normalize cg . fmap enrich_node_info $ ast
    pal = extractPal cg ast'
    ds = extractVarMap ast'
  in
    (ast', pal, ds)

enableBreakpoints :: CTranslUnit' -> CTranslUnit' -- {{{1
enableBreakpoints = everywhere (mkT tStat `extT` tInitDecl)
  where
    tStat :: CStat' -> CStat'
    tStat o@(CCompound _ _ _) = o
    tStat o@(CExpr Nothing _) = o
    tStat o@(CLabel _ _ _ _) = o
    tStat s = amap traceLocation s

    tInitDecl :: CDecl' -> CDecl'
    tInitDecl (CDecl x1 decls x2) = CDecl x1 (map tr decls) x2
      where
        tr (y1, Just y2, y3) = (y1, Just (amap traceLocation y2), y3)
        tr x                 = x

    traceLocation x = x {enLocation = True}

extractPal :: CallGraph -> CTranslUnit' -> CTranslUnit' -- {{{1
extractPal cg (CTranslUnit ds _) = CTranslUnit (map CDeclExt ds') un
  where
  bf = Set.fromList $ blocking_functions cg
  frames = Set.map frameType bf
  ds' = everything (++) (mkQ [] query) ds
  query (CDeclExt cd@(CDecl _ [_] _))
    | symbol cd `Set.member` bf = [cd]
    | symbol cd `Set.member` frames = [cd]
    | otherwise = []
  query _ = []

extractVarMap :: CTranslUnit' -> VarMap -- {{{1
extractVarMap _ = []
