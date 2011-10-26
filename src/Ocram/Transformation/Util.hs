module Ocram.Transformation.Util 
-- exports {{{1
(
  ident, un
) where

-- imports {{{1
import Language.C.Data.Ident (Ident(Ident))
import Language.C.Data.Node (undefNode, NodeInfo)

-- ident {{{1
ident :: String -> Ident
ident s = Ident s 0 un

un :: NodeInfo
un = undefNode
