module Ocram.Test.Tests.Filter.Constraints.FunctionPointer (
	tests
) where

import Ocram.Analysis (determineBlockingFunctions, getFunctions, determineCallGraph, determineCriticalFunctions)
import Ocram.Filter.Constraints (getErrorCodes)
import Ocram.Test.Tests.Filter.Utils (runTests)

reduce ast =
	case criticalFunctions ast of
		Left e -> error e
		Right cf -> getErrorCodes cf ast

criticalFunctions input_ast = do
	blocking_functions <- determineBlockingFunctions input_ast
	function_map <- getFunctions input_ast
	call_graph <- determineCallGraph input_ast function_map blocking_functions
	determineCriticalFunctions call_graph function_map blocking_functions

tests = runTests "function pointer" reduce [
	("__attribute__((tc_blocking)) void foo(); void bar(void*); void baz() { bar(&foo); }", [1])
	]
