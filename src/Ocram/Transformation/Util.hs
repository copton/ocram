module Ocram.Transformation.Util 
-- exports {{{1
(
	ident, un
) where

-- imports {{{1
import Language.C.Data.Ident (Ident(Ident))
import Language.C.Data.Node (undefNode)

-- ident {{{1
ident s = Ident s 0 un
un = undefNode
