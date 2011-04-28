module Ocram.Test.Tests.Filter.Sanity (
	tests
) where

import Ocram.Filter.Sanity (getErrorCodes)
import Ocram.Test.Tests.Filter.Utils (runTests)
import Ocram.Test.Lib (createContext)

reduce code = getErrorCodes $ createContext code Nothing

tests = runTests "Sanity" reduce [
		("void foo { }", [1])
	]

