module Ocram.Test.Tests (
	tests
) where

import qualified Ocram.Test.Tests.Symbol as A
import qualified Ocram.Test.Tests.Analysis as B
import qualified Ocram.Test.Tests.Visitor as C
import qualified Ocram.Test.Tests.Filter as D
import qualified Ocram.Test.Tests.Transformation as E
import Test.HUnit

tests = TestList [A.tests, B.tests, C.tests, D.tests, E.tests]
