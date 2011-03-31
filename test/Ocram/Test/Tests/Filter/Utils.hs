module Ocram.Test.Tests.Filter.Utils (
	runTests
) where

import Ocram.Types (RawAst)
import Ocram.Test.Lib (parse')
import Test.HUnit (Test(TestLabel,TestCase,TestList), assertEqual)
import Data.List (sort)

runTest :: (RawAst -> [Int]) -> (Int, (String, [Int])) -> Test
runTest reduce (number, (code, expected)) = TestCase $ assertEqual name (sort expected) (sort errors) 
	where
		name = "test" ++ show number
		errors = reduce $ parse' code

runTests :: String -> (RawAst -> [Int]) -> [(String, [Int])] -> Test
runTests label reduce tests = TestLabel label $ TestList $ map (runTest reduce) $ zip [1..] tests
