module Ocram.Test.Tests.Analysis.Utils (
	runTest, runTests
) where

import Ocram.Types (Result)
import Ocram.Test.Lib (parse)
import Test.HUnit (Test(TestLabel,TestCase,TestList), assertEqual)

runTest :: (Eq a, Show a) => (String -> Result a) -> (Int, (String, a)) -> Test
runTest reduce (number, (code, expected)) = TestCase $ assertEqual name expected result
	where
		name = "test" ++ show number
		result = case reduce code of
			Left e -> error e
			Right r -> r

runTests :: (Eq a, Show a) => String -> (String -> Result a) -> [(String, a)] -> Test
runTests label reduce tests = TestLabel label $ TestList $ map (runTest reduce) $ zip [1..] tests
