module Ocram.Test.Tests.Filter.Constraints (
	tests
) where

import Ocram.Filter.Constraints (getErrorCodes)
import Ocram.Test.Tests.Filter.Utils (runTests)
import Ocram.Test.Lib (createContext, paste)

tests = runTests "Constraints" reduce $ threads ++ function_pointer

reduce code = getErrorCodes (createContext code Nothing)

threads = [
	("", [2]),
	("__attribute__((tc_blocking)) void foo();", [2]),
	([$paste|
		__attribute__((tc_blocking)) void foo(); 
	  __attribute__((tc_run_thread)) void baz() { foo(); }
	|], [])
	]

function_pointer = [
	([$paste|
		__attribute__((tc_blocking)) void foo(); 
		void bar(void*); 
		__attribute__((tc_run_thread)) void baz() { 
			bar(&foo); 
			foo();
		}
	|], [1])
	]
