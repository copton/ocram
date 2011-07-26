module Ocram.Test.Main (main) where

import Ocram.Test.Tests (tests)
import Test.HUnit (runTestText, putTextToHandle, Counts(errors, failures))
import System.IO (stderr)
import System.Exit (exitWith, ExitCode(ExitFailure))

main = do
	(count, _ ) <- runTestText (putTextToHandle stderr False) tests
	if errors count > 0 || failures count > 0
		then
			exitWith $ ExitFailure 1
		else
			return ()
