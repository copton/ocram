module Ocram.Test.Tests.Filter (
	tests
) where

import qualified Ocram.Test.Tests.Filter.Sanity as A
import qualified Ocram.Test.Tests.Filter.Constraints as B
import Test.HUnit (Test(TestLabel, TestList))

tests = TestLabel "Sanity" $ TestList [A.tests, B.tests]
