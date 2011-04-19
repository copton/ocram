module Ocram.Test.Tests.Filter.Sanity.Test1 (
	tests
) where

import Ocram.Filter.Sanity (getErrorCodes)
import Ocram.Test.Tests.Filter.Utils (runTests)
import Ocram.Test.Lib (createContext)

reduce code = getErrorCodes $ createContext code Nothing

tests = runTests "Test1" reduce [
		("void foo { }", [1])
	]

