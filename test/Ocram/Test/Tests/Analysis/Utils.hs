module Ocram.Test.Tests.Analysis.Utils where

-- imports {{{1
import Ocram.Types
import Ocram.Test.Lib (TErrorCodes, TCode)
import Ocram.Test.Tests.Analysis.Types
import Ocram.Test.Tests.Analysis.TestCases (test_cases)
import Ocram.Test.Lib (parse)

import Test.HUnit (Test(TestLabel,TestCase,TestList), assertEqual)

import Data.List (isPrefixOf)

-- runTests :: (Eq o, Show o) => TestType -> (Ast -> i -> o) -> (TCase -> (i, o)) -> Test {{{1
runTests :: (Eq o, Show o) => TestType -> (Ast -> i -> o) -> (TCase -> (i, o)) -> Test
runTests test_type execute setup = TestLabel (show test_type) $ TestList $ map (runTest execute) $ zip [0..] $ map prepare $ filter type_filter test_cases
	where
		prepare tc = (tcCode tc, setup tc)
		type_filter tc = test_type `notElem` (tcExclude tc)

runTest :: (Eq o, Show o) => (Ast -> i -> o) -> (Int, (TCode, (i, o))) -> Test
runTest execute (number, (code, (input, output))) = TestCase $ assertEqual name output result
	where
		name = "test" ++ show number
		ast = parse code
		result = execute ast input

-- extractErrorCodes :: String -> TErrorCodes {{{1
extractErrorCodes :: String -> TErrorCodes
extractErrorCodes text = map extract $ filter ("### Error:" `isPrefixOf`) $ lines text
	where
		extract line = read $ takeWhile (/=')') $ tail $ dropWhile (/='(') line
