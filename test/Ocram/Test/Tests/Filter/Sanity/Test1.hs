module Ocram.Test.Tests.Filter.Sanity.Test1 (
	tests
) where

import Ocram.Filter.Sanity (getErrorCodes)
import Ocram.Test.Tests.Filter.Utils (runTests)

tests = runTests "Test1" getErrorCodes [
		("void foo { }", [1])
	]

