module Ocram.Analysis 
-- export {{{1
(
    analysis
  , module Ocram.Analysis.CallGraph
  , module Ocram.Analysis.Filter
  , CallGraph
) where

-- import {{{1
import Ocram.Analysis.CallGraph
import Ocram.Analysis.Filter
import Ocram.Analysis.Types (CallGraph)
import Ocram.Types (Ast)
import Ocram.Text (OcramError)

-- analysis :: Ast -> Either [OcramError] CallGraph {{{1
analysis :: Ast -> Either [OcramError] CallGraph
analysis ast = do
  check_sanity ast
  let cg = call_graph ast
  check_constraints ast cg
  return $ cg
