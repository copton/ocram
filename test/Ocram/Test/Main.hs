module Ocram.Test.Main (main) where

import Ocram.Test.Tests (tests)
import Test.HUnit (runTestTT)

main = runTestTT tests >> return ()
