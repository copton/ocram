module Tests.Analysis (
    tests
) where

import qualified Tests.Analysis.StartRoutines
import qualified Tests.Analysis.FunctionMap
import qualified Tests.Analysis.CallGraph

import Test.HUnit

tests :: Test
tests = TestLabel "Analysis" $ TestList [
            Tests.Analysis.StartRoutines.tests, 
            Tests.Analysis.FunctionMap.tests,
            Tests.Analysis.CallGraph.tests
        ]
