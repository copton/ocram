module Ocram.Analysis.Test (tests) where

import qualified Ocram.Analysis.Filter.Test as A
import qualified Ocram.Analysis.Fgl.Test as B
import qualified Ocram.Analysis.CallGraph.Test as C
import Test.Framework (testGroup, Test)

tests :: Test -- {{{1
tests = testGroup "Analysis" [A.tests, B.tests, C.tests]
