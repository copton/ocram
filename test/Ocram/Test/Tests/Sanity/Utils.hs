module Ocram.Test.Tests.Sanity.Utils (
	runTests
) where

import Ocram.Test.Lib (getAst)
import Test.HUnit (Test(TestLabel,TestCase,TestList), assertEqual)
import Data.List (sort)
import Ocram.Sanity (getErrorIds)

runTest :: (Int, (String, [Int])) -> Test
runTest (number, (code, expected)) = TestCase $ assertEqual name (sort expected) (sort errors) 
	where
		name = "test" ++ show number
		errors = getErrorIds $ getAst code

runTests :: String -> [(String, [Int])] -> Test
runTests label tests = TestLabel label $ TestList $ map runTest $ zip [1..] tests
