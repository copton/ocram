module Ocram.Analysis 
-- export {{{1
(
		analysis
	, CallGraph
) where

-- import {{{1
import Ocram.Analysis.Functions (CallGraph, call_graph)
import Ocram.Analysis.Filter (check_sanity, check_constraints)
import Ocram.Types (Ast)
import Ocram.Text (OcramError)

-- analysis :: Ast -> Either [OcramError] CallGraph {{{1
analysis :: Ast -> Either [OcramError] CallGraph
analysis ast = do
	check_sanity ast
	let cg = call_graph ast
	check_constraints ast cg
	return $ cg
