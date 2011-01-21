module Analysis.StartRoutine (
    findStartRoutines
) where

import Language.C.Syntax.AST
import Language.C.Data.Ident 

findStartRoutines :: CTranslationUnit a -> [CFunctionDef a]
findStartRoutines (CTranslUnit decls _) = map (\(CFDefExt x) -> x) $ filter isStartRoutine decls

isStartRoutine :: CExternalDeclaration a -> Bool
isStartRoutine (CFDefExt (CFunDef [CTypeSpec (CTypeDef (Ident "TC_RUN_THREAD" _ _) _)] _ _ _ _)) = True 
isStartRoutine  _ = False

