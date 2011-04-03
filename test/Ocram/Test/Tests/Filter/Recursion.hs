module Ocram.Test.Tests.Filter.Recursion (
	tests
) where

import qualified Ocram.Test.Tests.Filter.Recursion.Test1 as A
import Test.HUnit

tests = TestLabel "Recursion" $ TestList [A.tests]

