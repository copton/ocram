module Ocram.Test.Tests.Filter.Constraints.Threads (
tests
) where

import Ocram.Filter.Constraints (getErrorCodes)
import Ocram.Test.Tests.Filter.Utils (runTests)
import Ocram.Test.Lib (createContext, paste)

reduce code = getErrorCodes (createContext code Nothing)

tests = runTests "threads" reduce [
	("", [2]),
	("__attribute__((tc_blocking)) void foo();", [2]),
	([$paste|
		__attribute__((tc_blocking)) void foo(); 
	  __attribute__((tc_run_thread)) void baz() { }
	|], [])
	]
