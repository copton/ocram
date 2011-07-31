module Ocram.Test.Tests.Analysis.TestCases (
	test_cases
) where

import Ocram.Test.Lib (paste)
import Ocram.Options (defaultOptions)
import Ocram.Test.Tests.Analysis.Types

test_cases :: [TCase]
test_cases = [
-- empty code {{{2
		TCase 
			""
			defaultOptions
			[]
			[]
			[]
			[]
			[]
			[]
			[]
			[]
-- finish {{{2
	]
