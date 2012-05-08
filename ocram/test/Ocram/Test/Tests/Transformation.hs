module Ocram.Test.Tests.Transformation (
	tests
) where

import qualified Ocram.Test.Tests.Transformation.UniqueIdentifiers as A
import qualified Ocram.Test.Tests.Transformation.Normalize as B
import qualified Ocram.Test.Tests.Transformation.Inline as C

import Test.Framework (testGroup)

tests = testGroup "Transformation" [A.tests, B.tests, C.tests]
