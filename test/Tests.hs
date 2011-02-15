module Tests (
    tests
) where

import qualified Tests.Analysis
import Test.HUnit

tests = TestList [Tests.Analysis.tests]
