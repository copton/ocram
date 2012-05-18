module Ocram.Analysis 
-- export {{{1
(
    analysis
  , module Ocram.Analysis.CallGraph
  , module Ocram.Analysis.Filter
  , CallGraph
) where

-- import {{{1
import Language.C.Syntax.AST (CTranslUnit)
import Ocram.Analysis.CallGraph
import Ocram.Analysis.Filter
import Ocram.Analysis.Types (CallGraph)
import Ocram.Text (OcramError)

analysis :: CTranslUnit -> Either [OcramError] (CallGraph, Footprint) -- {{{1
analysis ast = do
  check_sanity ast
  let cg = call_graph ast
  check_constraints ast cg
  return (cg, footprint cg)
