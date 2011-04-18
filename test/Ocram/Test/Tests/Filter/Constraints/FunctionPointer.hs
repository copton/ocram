module Ocram.Test.Tests.Filter.Constraints.FunctionPointer (
	tests
) where

import Ocram.Filter.Constraints (getErrorCodes)
import Ocram.Test.Tests.Filter.Utils (runTests)
import Ocram.Test.Lib (createContext)

reduce code = getErrorCodes (createContext code Nothing)

tests = runTests "function pointer" reduce [
	("__attribute__((tc_blocking)) void foo(); void bar(void*); void baz() { bar(&foo); }", [1])
	]
