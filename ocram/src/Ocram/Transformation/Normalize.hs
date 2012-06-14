module Ocram.Transformation.Normalize
(
  normalize
) where

import Language.C.Syntax.AST
import Ocram.Analysis (critical_functions, CallGraph)
import Ocram.Debug
import Ocram.Symbols (symbol)
import Ocram.Transformation.Normalize.Internal
import Ocram.Transformation.Normalize.ShortCircuiting
import Ocram.Transformation.Normalize.UniqueIdentifiers

import qualified Data.Set as Set

normalize :: CallGraph -> CTranslUnit' -> CTranslUnit'
normalize cg ast@(CTranslUnit ds ni) = CTranslUnit ds' ni
  where
  ds' = (reverse . snd . foldl go (0, [])) ds
  cf = Set.fromList $ critical_functions cg

  go (tid, os) o@(CFDefExt fd)
    | Set.member (symbol fd) cf = (tid + 1, (CFDefExt . proc tid) fd : os)
    | otherwise = (tid, o : os)
  go (tid, os) o = (tid, o : os)

  proc tid = critical_statements tid cf ast . short_circuiting tid cf . defer_critical_initialization cf . wrap_dangling_statements . desugar_control_structures tid . explicit_return . unique_identifiers . unlist_declarations
