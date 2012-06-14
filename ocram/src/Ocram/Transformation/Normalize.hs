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
  ds' = map go ds
  cf = Set.fromList $ critical_functions cg

  go o@(CFDefExt fd)
    | Set.member (symbol fd) cf = (CFDefExt . proc) fd
    | otherwise = o   
  go o = o

  proc = critical_statements cf ast . short_circuiting cf . defer_critical_initialization cf . wrap_dangling_statements . desugar_control_structures . explicit_return . unique_identifiers . unlist_declarations
