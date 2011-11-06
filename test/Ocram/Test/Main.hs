module Ocram.Test.Main (main) where

import Test.HUnit (Test(TestList), runTestText, putTextToHandle, Counts(errors, failures))
import System.IO (stderr)
import System.Exit (exitWith, ExitCode(ExitFailure))

import qualified Ocram.Test.Tests.Symbol as A
import qualified Ocram.Test.Tests.Query as B
import qualified Ocram.Test.Tests.Analysis as C
import qualified Ocram.Test.Tests.Transformation as D

tests = TestList [A.tests, B.tests, C.tests, D.tests]

main = do
	(count, _ ) <- runTestText (putTextToHandle stderr False) tests
	if errors count > 0 || failures count > 0
		then
			exitWith $ ExitFailure 1
		else
			return ()
