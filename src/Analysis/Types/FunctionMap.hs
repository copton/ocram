module Analysis.Types.FunctionMap (
    FunctionMap
) where

import Data.Map as Map
import Language.C.Syntax.AST (CFunDef)

type FunctionMap = Map.Map String CFunDef
