module Ocram.Analysis.Algorithms.StartRoutines (
    findStartRoutines
) where

import Language.C.Syntax.AST
import Language.C.Data.Ident 
import Ocram.Context
import Ocram.Analysis.Types.StartRoutines
import Data.Map (elems)
import Ocram.Analysis.Types.FunctionMap (functionId')

findStartRoutines :: Context -> StartRoutines
findStartRoutines ctx = map functionId' $ filter isStartRoutine $ elems $ ctxFunctionMap ctx

isStartRoutine :: CFunDef -> Bool
isStartRoutine (CFunDef specs _ _ _ _) = any checkAttr specs 

checkAttr (CTypeQual (CAttrQual (CAttr (Ident "tc_run_thread" _ _) [] _))) = True
checkAttr _ = False
