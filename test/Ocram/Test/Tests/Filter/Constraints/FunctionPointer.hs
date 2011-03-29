module Ocram.Test.Tests.Filter.Constraints.FunctionPointer (
	tests
) where

import Ocram.Analysis (determineBlockingFunctions, getFunctions, determineCallGraph, determineCriticalFunctions)
import Ocram.Filter.Constraints (getErrorCodes)
import Ocram.Test.Tests.Filter.Utils (runTests)

reduce :: RawAst -> [Int]
reduce raw_ast =
	case criticalFunctions raw_ast of
		Left e -> error e
		Right cf -> getErrorCodes cf ast

criticalFunctions raw_ast = do
	sane_ast <- checkSanity raw_ast
	blocking_functions <- determineBlockingFunctions sane_ast
	function_map <- getFunctions sane_ast
	call_graph <- determineCallGraph sane_ast function_map blocking_functions
	cyclefree
	determineCriticalFunctions call_graph function_map blocking_functions

tests = runTests "function pointer" reduce [
	("__attribute__((tc_blocking)) void foo(); void bar(void*); void baz() { bar(&foo); }", [1])
	]
