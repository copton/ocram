{-# LANGUAGE TemplateHaskell #-}
module Ocram.Symbols 
-- export {{{1
(
  Symbol, symbol
) where

-- import {{{1
import Language.C.Syntax.AST
import Language.C.Data.Ident
import Ocram.Util (abort)

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
  symbol _ = $abort "unexpected parameters"

instance CSymbol CDecl where
  symbol (CDecl _ [(Just declr, _, _)] _) = symbol declr
  symbol (CDecl [CTypeSpec (CSUType (CStruct _ (Just (Ident name _ _ )) _ _ _) _)] [] _) = name
  symbol (CDecl [CTypeSpec (CEnumType (CEnum (Just (Ident name _ _)) _ _ _) _)] [] _) = name
  symbol (CDecl [CTypeSpec (CEnumType (CEnum Nothing _ _ _) _)] [] _) = no_name
  symbol _ = $abort "unexpected parameters"

no_name :: String
no_name = "<<no_name>>"
