module Ocram.Test.Tests (
	tests
) where

import qualified Ocram.Test.Tests.Analysis as A
import qualified Ocram.Test.Tests.Visitor as B
import qualified Ocram.Test.Tests.Filter as C
import Test.HUnit

tests = TestList [A.tests, B.tests, C.tests]
