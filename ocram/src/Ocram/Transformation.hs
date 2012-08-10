module Ocram.Transformation
-- exports {{{1
(
  transformation
) where

-- imports {{{1
import Data.Generics (everything, everywhere, mkT, mkQ, extT)
import Language.C.Syntax.AST
import Language.C.Data.Node (nodeInfo, isUndefNode)
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
    ast' = (translate cg . enableBreakpoints . normalize cg . fmap enrich_node_info) ast
    pal = extractPal cg ast'
    ds = extractVarMap ast'
  in
    (ast', pal, ds)

enableBreakpoints :: CTranslUnit' -> CTranslUnit' -- {{{1
enableBreakpoints = everywhere (mkT tStat `extT` tDecl)
  where
    tStat :: CStat' -> CStat'
    tStat o@(CCompound _ _ _) = o
    tStat o@(CExpr Nothing _) = o
    tStat s = amap setBreakpoint s

    tDecl :: CDecl' -> CDecl'
    -- Decls with initializers
    tDecl o@(CDecl _ [(_, Just _, _)] _) = amap setBreakpoint o
    tDecl o = o

    setBreakpoint x
      | (isUndefNode . nodeInfo) x = x
      | otherwise = x {enBreakpoint = True}

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
