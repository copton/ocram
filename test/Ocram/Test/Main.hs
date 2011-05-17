module Ocram.Test.Main (main) where

import Ocram.Test.Tests (tests)
import Test.HUnit (runTestText, putTextToHandle)
import System.IO (stderr)

main = runTestText (putTextToHandle stderr False) tests >> return ()
