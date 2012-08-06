module Ruab.Test (runTests) where

-- imports {{{1
import Test.Framework (Test, testGroup, defaultMainWithArgs)

import qualified Ruab.Frontend.Test as A
import qualified Ruab.Backend.Test as B
import qualified Ruab.Core.Test as C

runTests :: [String] -> IO () -- {{{1
runTests = defaultMainWithArgs [tests]

tests :: Test -- {{{2
tests = testGroup "Ruab" [A.tests, B.tests, C.tests]
