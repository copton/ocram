module Ocram.Symbols 
-- export {{{1
(
  Symbol, symbol, reserved_identifier
) where

-- import {{{1
import Data.Char (isUpper)
import Language.C.Syntax.AST
import Language.C.Data.Ident
import Ocram.Util (abort, unexp)

-- symbol {{{1
type Symbol = String

class CSymbol s where
  symbol :: s -> Symbol

instance CSymbol String where
  symbol = id

instance CSymbol Ident where
  symbol (Ident name _ _) = name

instance CSymbol CFunDef where
  symbol (CFunDef _ declr  _ _ _ ) = symbol declr

instance CSymbol CDeclr where
  symbol (CDeclr (Just (Ident name _ _)) _ _ _ _) = name
  symbol x = $abort $ unexp x

instance CSymbol CDecl where
  symbol (CDecl _ [(Just declr, _, _)] _) = symbol declr
  symbol (CDecl [CTypeSpec (CSUType (CStruct _ (Just (Ident name _ _ )) _ _ _) _)] [] _) = name
  symbol (CDecl [CTypeSpec (CEnumType (CEnum (Just (Ident name _ _)) _ _ _) _)] [] _) = name
  symbol (CDecl [CTypeSpec (CEnumType (CEnum Nothing _ _ _) _)] [] _) = no_name
  symbol x = $abort $ unexp x

instance CSymbol CExtDecl where
  symbol (CDeclExt cd) = symbol cd
  symbol (CFDefExt fd) = symbol fd
  symbol x = $abort $ unexp x

no_name :: String
no_name = "<<no_name>>"

reserved_identifier :: String -> Bool
reserved_identifier ('_':'_': _) = True
reserved_identifier ('_':x:_) = isUpper x
reserved_identifier _ = False
