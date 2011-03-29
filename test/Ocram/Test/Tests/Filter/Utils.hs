module Ocram.Test.Tests.Filter.Utils (
	runTests
) where

import Ocram.Types (AST)
import Ocram.Test.Lib (getAst)
import Test.HUnit (Test(TestLabel,TestCase,TestList), assertEqual)
import Data.List (sort)

runTest :: (AST -> [Int]) -> (Int, (String, [Int])) -> Test
runTest reduce (number, (code, expected)) = TestCase $ assertEqual name (sort expected) (sort errors) 
	where
		name = "test" ++ show number
		errors = reduce $ getAst code

runTests :: String -> (AST -> [Int]) -> [(String, [Int])] -> Test
runTests label reduce tests = TestLabel label $ TestList $ map (runTest reduce) $ zip [1..] tests
