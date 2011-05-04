module Ocram.Transformation.Util 
-- exports {{{1
(
	ident, un
) where

-- imports {{{1
import Ocram.Types
import qualified Data.Set as Set
import Language.C.Data.Ident (Ident(Ident))
import Language.C.Data.Node (undefNode)
import Language.C.Syntax.AST
import Ocram.Names (blockingAttr, startRoutineAttr)

-- ident {{{1
ident s = Ident s 0 un
un = undefNode


