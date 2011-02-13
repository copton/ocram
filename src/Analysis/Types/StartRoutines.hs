module Analysis.Types.StartRoutines (
    StartRoutines
) where

import Language.C.Syntax.AST (CFunDef)

type StartRoutines = [CFunDef]
