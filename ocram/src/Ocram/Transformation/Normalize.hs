module Ocram.Transformation.Normalize
(
  normalize
) where

import Ocram.Analysis (critical_functions)
import Ocram.Transformation.Types (Transformation)
import Ocram.Transformation.Normalize.Internal
import Ocram.Transformation.Normalize.ShortCirtuiting
import Ocram.Transformation.Normalize.UniqueIdentifiers

import qualified Data.Set as Set

normalize :: Transformation
normalize cg (CTranslUnit ds ni) = return (CTranslUnit ds' ni)
  where
  ds' = map go ds
  cf = Set.fromList $ critical_functions cg

  go o@(CFDefExt fd)
    | Set.member (symbol fd) cf = CFDefExt $ fd
    | otherwise = o   
  go o = o

  proc = defer_critical_initialization . critical_statements cf . short_circuiting cf . desugar_control_structures . explicit_return . unique_identifiers . unlist_declarations . wrap_dangling_statements
