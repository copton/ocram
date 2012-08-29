module Ocram.Transformation.Util 
-- exports {{{1
(
  ident,
) where

-- imports {{{1
import Language.C.Data.Ident (internalIdent, Ident)

ident :: String -> Ident -- {{{1
ident = internalIdent 
--ident s = Ident s 0 undefNode
