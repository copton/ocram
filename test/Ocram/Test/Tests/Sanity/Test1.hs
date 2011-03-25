module Ocram.Test.Tests.Sanity.Test1 (
	tests
) where

import Ocram.Test.Tests.Sanity.Utils (runTests)

tests = runTests "Test1" [
		("void foo { }", [1])
	]

