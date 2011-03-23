module Ocram.Symbols (Symbol, symbol) where

import Language.C.Syntax.AST
import Language.C.Data.Ident

type Symbol = String

class CSymbol s where
	symbol :: s -> Symbol

instance CSymbol String where
	symbol = id

instance CSymbol CFunDef where
	symbol (CFunDef _ (CDeclr (Just (Ident name _ _)) _ _ _ _) _ _ _ ) = name
