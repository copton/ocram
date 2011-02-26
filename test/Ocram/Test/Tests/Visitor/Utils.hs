module Ocram.Test.Tests.Visitor.Utils (
	runTests
) where

import Ocram.Test.Lib.Parser (parse)
import Data.List (intersperse)
import Test.HUnit (Test(TestLabel,TestCase,TestList), assertEqual)
import Language.C.Pretty (pretty, Pretty)
import Language.C.Syntax.AST (CTranslUnit)

code strs = concat $ intersperse "\n" strs

runTest :: (Pretty b) => (Int, ([String], (CTranslUnit->b), [String])) -> Test
runTest (number, (input, transform, output)) = TestCase $ assertEqual name expected result
	where
		name = "test" ++ show number
		result = show $ pretty $ transform $ parse $ code input
		expected = code output

runTests :: (Pretty b) => String -> [([String],(CTranslUnit->b),[String])] -> Test
runTests label tests = TestLabel label $ TestList $ map runTest $ zip [1..] tests
