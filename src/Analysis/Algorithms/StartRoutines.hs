module Analysis.Algorithms.StartRoutines (
    findStartRoutines
) where

import Language.C.Syntax.AST
import Language.C.Data.Ident 
import Context
import Analysis.Types.StartRoutines
import Data.Map (toList)

findStartRoutines :: Context -> StartRoutines
findStartRoutines ctx = filter isStartRoutine $ functions $ ctxFunctionMap ctx
    where functions m = map snd $ toList m 

isStartRoutine :: CFunDef -> Bool
isStartRoutine (CFunDef [CTypeSpec (CTypeDef (Ident "TC_RUN_THREAD" _ _) _)] _ _ _ _) = True 
isStartRoutine  _ = False

