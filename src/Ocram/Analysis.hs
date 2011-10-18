module Ocram.Analysis 
-- export {{{1
(
		analysis
	, module Ocram.Analysis.Functions
	, module Ocram.Analysis.Filter
) where

-- import {{{1
import Ocram.Analysis.Functions
import Ocram.Analysis.Filter
import Ocram.Types (Ast)
import Ocram.Text (OcramError)

-- analysis :: Ast -> Either [OcramError] CallGraph {{{1
analysis :: Ast -> Either [OcramError] CallGraph
analysis ast = do
	check_sanity ast
	let cg = call_graph ast
	check_constraints ast cg
	return $ cg
