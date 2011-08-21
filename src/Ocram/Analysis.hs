module Ocram.Analysis (
	analysis,
	module Ocram.Analysis.BlockingFunctions,
	module Ocram.Analysis.CallGraph,
	module Ocram.Analysis.CriticalFunctions,
	module Ocram.Analysis.DefinedFunctions,
	module Ocram.Analysis.StartRoutines,
	module Ocram.Analysis.ADG,
	module Ocram.Analysis.Constraints,
	module Ocram.Analysis.Sanity
) where

import Ocram.Analysis.BlockingFunctions
import Ocram.Analysis.CallGraph
import Ocram.Analysis.CriticalFunctions
import Ocram.Analysis.DefinedFunctions
import Ocram.Analysis.StartRoutines
import Ocram.Analysis.ADG
import Ocram.Analysis.Constraints
import Ocram.Analysis.Sanity
import Ocram.Types

analysis :: Ast -> Either String Analysis
analysis ast = do
	check_sanity ast
	bf <- return $ blocking_functions ast
	df <- return $ defined_functions ast
	sr <- return $ start_routines df ast
	cg <- return $ call_graph df bf ast
	check_call_graph cg sr df ast
	cf <- return $ critical_functions cg bf ast
	check_constraints cf sr ast
	return $ Analysis bf df sr cg cf

