module Ocram.Transformation.Util (
	ident, un
) where

import Language.C.Data.Ident (Ident(Ident))
import Language.C.Data.Node (undefNode)

ident s = Ident s 0 un
un = undefNode
