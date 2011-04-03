module Ocram.Test.Tests.Filter (
	tests
) where

import qualified Ocram.Test.Tests.Filter.Sanity as A
import qualified Ocram.Test.Tests.Filter.Constraints as B
import qualified Ocram.Test.Tests.Filter.Recursion as C
import Test.HUnit (Test(TestLabel, TestList))

tests = TestLabel "Filter" $ TestList [A.tests, B.tests, C.tests]
