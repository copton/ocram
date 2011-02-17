module Ocram.Test.Tests (
    tests
) where

import qualified Ocram.Test.Tests.Analysis as A
import Test.HUnit

tests = TestList [A.tests]
