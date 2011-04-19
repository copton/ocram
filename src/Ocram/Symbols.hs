module Ocram.Symbols (symbol) where

import Ocram.Types (Symbol)
import Language.C.Syntax.AST
import Language.C.Data.Ident

class CSymbol s where
	symbol :: s -> Symbol

instance CSymbol String where
	symbol = id

instance CSymbol CFunDef where
	symbol (CFunDef _ (CDeclr (Just (Ident name _ _)) _ _ _ _) _ _ _ ) = name

instance CSymbol CDecl where
	symbol (CDecl _ [(Just (CDeclr (Just (Ident name _ _ )) _  _ _ _), _, _)] _) = name
	symbol (CDecl [CTypeSpec (CSUType (CStruct _ (Just (Ident name _ _ )) _ _ _) _)] [] _) = name
	symbol (CDecl [CTypeSpec (CEnumType (CEnum (Just (Ident name _ _)) _ _ _) _)] [] _) = name
	symbol (CDecl [CTypeSpec (CEnumType (CEnum Nothing _ _ _) _)] [] _) = no_name

no_name = "<<no_name>>"
