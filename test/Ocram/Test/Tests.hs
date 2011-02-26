module Ocram.Test.Tests (
	tests
) where

import qualified Ocram.Test.Tests.Analysis as A
import qualified Ocram.Test.Tests.Visitor as B
import Test.HUnit

tests = TestList [A.tests, B.tests]
