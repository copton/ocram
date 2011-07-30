module Ocram.Test.Tests.Analysis.BlockingFunctions (
	tests
) where

import Ocram.Types
import Ocram.Test.Lib
import Ocram.Analysis.BlockingFunctions
import qualified Data.Set as Set

reduce :: String -> ER [String]
reduce code = 
	return . Set.toList =<< blocking_functions (parse code) 
	
tests = runTests "BlockingFunctions" reduce [
	 ("void foo() { }", [])
	,("", [])
	,("void foo();", [])
	,("__attribute__((tc_blocking)) void foo();", ["foo"])
	,("__attribute__((tc_blocking)) int foo();", ["foo"])
	,("__attribute__((tc_blocking)) int foo(char);", ["foo"])
	,("__attribute__((tc_blocking)) int foo(double,...);", ["foo"])
	,([$paste|
		__attribute__((tc_blocking)) int block(int i);

		__attribute__((tc_run_thread)) void start() 
		{
			int i;
			i = block(i);
		}
	|], ["block"])
	]
