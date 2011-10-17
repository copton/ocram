module Ocram.Test.Tests.Query 
-- exports {{{1
(
	tests
) where

-- imports {{{1
import Ocram.Test.Lib

import Test.HUnit (Test(TestLabel,TestCase,TestList), assertEqual)

-- tests
root = "a"
tests = runTests [
		([(root, "b")], [root, "b"]),
		([(root, "b"), (root, "c")], [root, "b", "c"])
	]

-- runTests :: [(TCallGraphShort, TCallChain)] -> Test {{{1
runTests :: [(TCallGraphShort, TCallChain)] -> Test
runTests test_cases = TestLabel "CallChain" $ TestList $ map runTest $ zip [1..] test_cases

runTest :: (Int, (TCallGraphShort, TCallChain)) -> Test
runTest (number, (cg, cc)) = TestCase $ assertEqual name expected result
	where
		name = "test" ++ show number
		expected = (reduce cc :: [String])
		result = reduce $ getCallChain (enrich cg) (enrich root)
