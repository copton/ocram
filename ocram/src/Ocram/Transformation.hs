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
    ast' = (translate cg . normalize cg . enableLocationTracing . fmap enrich_node_info) ast
    pal = extractPal cg ast'
    ds = extractVarMap ast'
  in
    (ast', pal, ds)

enableLocationTracing :: CTranslUnit' -> CTranslUnit' -- {{{1
enableLocationTracing = everywhere (mkT tExtDecl `extT` tStat `extT` tExpr)
  where
    tExtDecl :: CExtDecl' -> CExtDecl' 
    tExtDecl (CFDefExt fd) = CFDefExt (amap enableTrace fd)
    tExtDecl x = x

    tStat :: CStat' -> CStat'
    tStat = amap enableTrace

    tExpr :: CExpr' -> CExpr'
    tExpr o@(CCall _ _ _) = amap enableTrace o
    tExpr o = o

    enableTrace x = x {enTraceLocation = True}

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
