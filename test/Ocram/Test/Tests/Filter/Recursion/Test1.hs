module Ocram.Test.Tests.Filter.Recursion.Test1 (
	tests
) where

import Ocram.Filter.Recursion (getErrorCodes)
import Ocram.Test.Tests.Filter.Utils (runTests)
import Ocram.Types (RawAst)
import Ocram.Analysis (determineBlockingFunctions, getFunctions ,findStartRoutines, determineCallGraph)
import Ocram.Filter (checkSanity)

reduce :: RawAst -> [Int]
reduce raw_ast = case getErrorCodes' raw_ast of
	Left e -> error e
	Right x -> x

getErrorCodes' raw_ast = do
	sane_ast <- checkSanity raw_ast
	blocking_functions <- determineBlockingFunctions sane_ast
	function_map <- getFunctions sane_ast
	start_routines <- findStartRoutines function_map
	call_graph <- determineCallGraph sane_ast function_map blocking_functions
	return $ getErrorCodes sane_ast call_graph start_routines function_map

tests = runTests "Test1" reduce [
		("void foo(){bar();} void bar(){foo();}", []),
		("__attribute__((tc_blocking)) void block(); __attribute__((tc_run_thread)) void start(){rec();} void rec() {block(); start();}", [1])
	]
