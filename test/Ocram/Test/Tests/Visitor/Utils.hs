module Ocram.Test.Tests.Visitor.Utils (
	runTests
) where

import Ocram.Types
import Ocram.Test.Lib
import Data.List (intersperse)
import Test.HUnit (Test(TestLabel,TestCase,TestList), assertEqual)
import Language.C.Pretty (pretty, Pretty)

runTest :: (Pretty b) => (Int, (String, (Ast->b), String)) -> Test
runTest (number, (input, transform, output)) = TestCase $ assertEqual name expected result
	where
		name = "test" ++ show number
		result = show $ pretty $ transform $ parse input
		expected = show $ pretty $ parse $ output

runTests :: (Pretty b) => String -> [(String,(Ast->b),String)] -> Test
runTests label tests = TestLabel label $ TestList $ map runTest $ zip [1..] tests
