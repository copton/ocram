module Ocram.Transformation.Inline.Normalize
-- export {{{1
(
  normalize
) where

-- import {{{1
import Ocram.Transformation.Inline.Types (WR)
import Ocram.Types (Ast)
import Control.Monad.Reader (ask)
import Ocram.Transformation.Util (ident, map_critical_functions)
import Language.C.Syntax.AST
import Language.C.Data.Node (nodeInfo)
import Ocram.Visitor (DownVisitor, UpVisitor, ListVisitor(..), traverseCFunDef)

normalize :: Ast -> WR Ast -- {{{1
normalize ast = return ast
