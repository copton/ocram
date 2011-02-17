module Ocram.Util.Names (
    functionName
) where

import Language.C.Syntax.AST
import Language.C.Data.Ident

functionName :: CFunctionDef a -> String
functionName (CFunDef _ (CDeclr (Just (Ident name _ _)) _ _ _ _) _ _ _ ) = name
