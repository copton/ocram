module Ocram.Test.Tests.Analysis (
	tests
) where

import qualified Ocram.Test.Tests.Analysis.StartRoutines as A
import qualified Ocram.Test.Tests.Analysis.DefinedFunctions as B
import qualified Ocram.Test.Tests.Analysis.CallGraph as C
import qualified Ocram.Test.Tests.Analysis.BlockingFunctions as D
import qualified Ocram.Test.Tests.Analysis.CriticalFunctions as E

import Test.HUnit

tests = TestLabel "Analysis" $ TestList [ A.tests, B.tests, C.tests, D.tests, E.tests ]
