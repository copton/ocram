module Util.Filter (
    funDefs
) where

import Language.C.Syntax.AST

funDefs :: CTranslUnit -> [CFunDef]
funDefs (CTranslUnit decls _) = map (\(CFDefExt x) -> x) $ filter isFunDef decls 
    where isFunDef (CFDefExt (CFunDef _ _ _ _ _)) = True
          isFunDef _ = False
