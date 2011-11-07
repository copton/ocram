module Ocram.Transformation.Inline.Normalize
-- export {{{1
(
  normalize
) where

-- import {{{1
import Ocram.Transformation.Inline.Types (Transformation)
import Ocram.Transformation.Util (map_critical_functions)
import Language.C.Syntax.AST
import Language.C.Data.Node (nodeInfo)

normalize :: Transformation -- {{{1
normalize cg ast = return $ map_critical_functions cg ast id
