module Ocram.Test.Tests.Filter.Constraints.FunctionPointer (
	tests
) where

import Ocram.Filter.Constraints (getErrorCodes)
import Ocram.Test.Tests.Filter.Utils (runTests)
import Ocram.Test.Lib (createContext, paste)

reduce code = getErrorCodes (createContext code Nothing)

tests = runTests "function pointer" reduce [
	([$paste|
		__attribute__((tc_blocking)) void foo(); 
		void bar(void*); 
		__attribute__((tc_run_thread)) void baz() { 
			bar(&foo); 
			foo();
		}
	|], [1])
	]
