module Ocram.Test.Tests.Symbol.Util (
	runTests
) where

import Test.HUnit (Test(TestLabel,TestCase,TestList), assertEqual)
import Ocram.Types (getAst, Ast, Symbol)
import Ocram.Test.Lib (parse')


runTests :: String -> (Ast -> String) -> [(String, Symbol)] -> Test
runTests label reduce tests = TestLabel label $ TestList $ map (runTest reduce) $ zip [1..] tests

runTest :: (Ast -> String) -> (Int, (String, String)) -> Test
runTest reduce (number, (code, expected)) = TestCase $ assertEqual name expected symbol
	where
		name = "test" ++ show number
		symbol = reduce $ getAst $ parse' code
