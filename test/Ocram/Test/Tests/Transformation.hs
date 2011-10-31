module Ocram.Test.Tests.Transformation (
	tests
) where

import qualified Ocram.Test.Tests.Transformation.UniqueIdentifiers as A
import qualified Ocram.Test.Tests.Transformation.Inline as B

import Test.HUnit

tests = TestLabel "Transformation" $ TestList [A.tests, B.tests]
