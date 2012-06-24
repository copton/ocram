module Ruab.Test (runTests) where

-- imports {{{1
import Test.Framework (Test, testGroup, defaultMainWithArgs)

import qualified Ruab.Frontend.Test as A

runTests :: [String] -> IO () -- {{{1
runTests = defaultMainWithArgs [tests]

tests :: Test -- {{{2
tests = testGroup "Ruab" [A.tests]

