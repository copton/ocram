module Ocram.Test.Tests.Visitor (
	tests
) where

import qualified Ocram.Test.Tests.Visitor.TranslUnit as A
import Test.HUnit (Test(TestLabel, TestList))

tests = TestLabel "Visitor" $ TestList [A.tests]
