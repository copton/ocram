module Ocram.Test.Tests.Query 
-- exports {{{1
(
	tests
) where

-- imports {{{1
import Data.Maybe (isJust, fromJust)
import Ocram.Analysis (get_call_chain)
import Ocram.Symbols (Symbol)
import Ocram.Test.Lib
import Test.HUnit (Test(TestLabel,TestCase,TestList), (@?), (@=?))

-- tests {{{1
tests = runTests [
		([("a", "b")], ("a", "b"), ["a", "b"])
	,	([("a", "b"), ("a", "c")], ("a", "b"), ["a", "b"])
	,	([("a", "b"), ("a", "c")], ("a", "c"), ["a", "c"])
	]

runTests :: [(TCallGraph, (Symbol, Symbol), TCallChain)] -> Test
runTests test_cases = TestLabel "CallChain" $ TestList $ map runTest $ zip [1..] test_cases

runTest :: (Int, (TCallGraph, (Symbol, Symbol), TCallChain)) -> Test
runTest (number, (cg, (start, end), cc)) = TestCase assertion
	where
		name = "test" ++ show number
		assertion = do
			let result = get_call_chain (enrich cg) (enrich start) (enrich end)
			isJust result @? "could not determine call chain."
			cc @=? (reduce $ fromJust result)
