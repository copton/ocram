module Tests.Analysis (
    tests
) where

import qualified Tests.Analysis.StartRoutines
import Test.HUnit

tests :: Test
tests = TestLabel "Analysis" $ TestList [Tests.Analysis.StartRoutines.tests]
