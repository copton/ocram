module Ocram.Test.Tests.Visitor (
	tests
) where

import qualified Ocram.Test.Tests.Visitor.TranslUnit as A
import qualified Ocram.Test.Tests.Visitor.Stat as B
import Test.HUnit (Test(TestLabel, TestList))

tests = TestLabel "Visitor" $ TestList [A.tests, B.tests]

