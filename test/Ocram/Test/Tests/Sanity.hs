module Ocram.Test.Tests.Sanity (
	tests
) where

import qualified Ocram.Test.Tests.Sanity.Test1 as A
import Test.HUnit (Test(TestLabel, TestList))

tests = TestLabel "Sanity" $ TestList [A.tests]
