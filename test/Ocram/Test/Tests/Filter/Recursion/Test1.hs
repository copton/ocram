module Ocram.Test.Tests.Filter.Recursion.Test1 (
	tests
) where

import Ocram.Filter.Recursion (getErrorCodes)
import Ocram.Test.Tests.Filter.Utils (runTests)
import Ocram.Test.Lib (createContext)

reduce code = getErrorCodes (createContext code Nothing)

tests = runTests "Test1" reduce [
		("void foo(){bar();} void bar(){foo();}", []),
		("__attribute__((tc_blocking)) void block(); __attribute__((tc_run_thread)) void start(){rec();} void rec() {block(); start();}", [1])
	]
