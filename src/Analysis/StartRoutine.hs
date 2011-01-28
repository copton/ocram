module Analysis.StartRoutine (
    findStartRoutines
) where

import Language.C.Syntax.AST
import Language.C.Data.Ident 
import Util.Filter (funDefs)

findStartRoutines :: CTranslUnit -> [CFunDef]
findStartRoutines ctu = filter isStartRoutine $ funDefs ctu

isStartRoutine :: CFunDef -> Bool
isStartRoutine (CFunDef [CTypeSpec (CTypeDef (Ident "TC_RUN_THREAD" _ _) _)] _ _ _ _) = True 
isStartRoutine  _ = False

