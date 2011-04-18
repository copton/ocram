module Ocram.Context (
	context
) where

import Ocram.Types
import Ocram.Analysis
import Ocram.Filter
import Ocram.Transformation

context :: Options -> RawAst -> Context
context options raw_ast = ctx
	where
		ctx = Context options raw_ast sane_ast blocking_functions defined_functions start_routines call_graph cyclefree_ast critical_functions valid_ast revised_ast function_infos stackless_ast output_ast
		sane_ast = checkSanity ctx
		blocking_functions = determineBlockingFunctions ctx
		defined_functions = collectDefinedFunctions ctx
		start_routines = findStartRoutines ctx
		call_graph = determineCallGraph ctx
		cyclefree_ast = checkRecursion ctx
		critical_functions = determineCriticalFunctions ctx
		valid_ast = checkConstraints ctx
		revised_ast = removeAttributes ctx
		dfResult = transformDataFlow ctx
		function_infos = fmap fst dfResult
		stackless_ast = fmap snd dfResult
		output_ast =transformControlFlow ctx
