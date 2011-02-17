module Ocram.Test.Tests.Analysis (
	tests
) where

import qualified Ocram.Test.Tests.Analysis.StartRoutines as A
import qualified Ocram.Test.Tests.Analysis.FunctionMap as B
import qualified Ocram.Test.Tests.Analysis.CallGraph as C

import Test.HUnit

tests = TestLabel "Analysis" $ TestList [ A.tests, B.tests, C.tests ]
