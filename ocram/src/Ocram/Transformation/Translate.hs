module Ocram.Transformation.Translate
(
  translate
) where

import Ocram.Debug (CTranslUnit')
import Ocram.Analysis (CallGraph, critical_functions)
import Ocram.Transformation.Translate.Internal
import Ocram.Transformation.Translate.ThreadFunctions

import qualified Data.Set as Set

translate :: CallGraph -> CTranslUnit' -> CTranslUnit'
translate cg =
  let
    cf = Set.fromList $ critical_functions cg
  in
    remove_critical_functions cg . add_thread_functions cg . add_blocking_function_decls . add_tstacks cg

-- TODO: mangle identifiers (vars, labels) with function names
