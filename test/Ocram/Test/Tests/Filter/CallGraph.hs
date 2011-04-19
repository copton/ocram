module Ocram.Test.Tests.Filter.CallGraph (
	tests
) where

import qualified Ocram.Test.Tests.Filter.CallGraph.Test1 as A
import Test.HUnit

tests = TestLabel "CallGraph" $ TestList [A.tests]

