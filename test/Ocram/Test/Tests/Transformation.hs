module Ocram.Test.Tests.Transformation (
	tests
) where

import qualified Ocram.Test.Tests.Transformation.UniqueIdentifiers as A
import qualified Ocram.Test.Tests.Transformation.Normalize as B
import qualified Ocram.Test.Tests.Transformation.Inline as C

import Test.HUnit

tests = TestLabel "Transformation" $ TestList [A.tests, B.tests, C.tests]
