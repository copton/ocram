module Ocram.Transformation.Translate
(
  translate
) where

import Ocram.Analysis (CallGraph)
import Ocram.Transformation.Translate.Internal
import Ocram.Transformation.Translate.ThreadFunctions
import Ocram.Transformation.Types (CTranslUnit')

translate :: CallGraph -> CTranslUnit' -> CTranslUnit'
translate cg = remove_critical_functions cg . add_thread_functions cg . add_blocking_function_decls . add_tstacks cg
