module Ocram.Test.Tests.Visitor.Utils (
	runTests
) where

import Ocram.Test.Lib (parse')
import Ocram.Types (RawAst)
import Data.List (intersperse)
import Test.HUnit (Test(TestLabel,TestCase,TestList), assertEqual)
import Language.C.Pretty (pretty, Pretty)

code strs = concat $ intersperse "\n" strs

runTest :: (Pretty b) => (Int, ([String], (RawAst->b), [String])) -> Test
runTest (number, (input, transform, output)) = TestCase $ assertEqual name expected result
	where
		name = "test" ++ show number
		result = show $ pretty $ transform $ parse' $ code input
		expected = code output

runTests :: (Pretty b) => String -> [([String],(RawAst->b),[String])] -> Test
runTests label tests = TestLabel label $ TestList $ map runTest $ zip [1..] tests
