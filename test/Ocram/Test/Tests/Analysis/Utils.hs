module Ocram.Test.Tests.Analysis.Utils where

import Ocram.Types
import Ocram.Test.Tests.Analysis.Types
import Ocram.Test.Tests.Analysis.TestCases (test_cases)
import Ocram.Test.Lib (parse)
import Ocram.Symbols (symbol)

import Test.HUnit (Test(TestLabel,TestCase,TestList), assertEqual)

import qualified Data.Set as Set
import qualified Data.Map as Map

runTest :: (Eq o, Show o) => (Ast -> i -> ER o) -> (Int, (TCode, Options, (i, o))) -> Test
runTest execute (number, (code, options, (input, output))) = TestCase $ assertEqual name output result
	where
		name = "test" ++ show number
		ast = parse code
		result = case execER options (execute ast input) of
			Left e -> error e
			Right x -> x

runTests :: (Eq o, Show o) => String -> (Ast -> i -> ER o) -> (TCase -> (i, o)) -> Test
runTests label execute setup = TestLabel label $ TestList $ map (runTest execute) $ zip [1..] $ map prepare test_cases
	where
		prepare tc = (tcCode tc, tcOptions tc, setup tc)

class TestData d t where
	reduce :: d -> t
	enrich :: t -> d

instance TestData (Set.Set Symbol) [String] where
	reduce = Set.toList
	enrich = Set.fromList

instance TestData CallGraph TCallGraph where
	reduce cg = map decompose (Map.toList cg)
		where
			decompose (function, (Entry callers callees)) =
				(function, Set.toList callers, Set.toList callees)

	enrich cg = foldl construct Map.empty cg
		where
			construct m (function, callers, callees) =
				Map.insert (symbol function) (Entry (convert callers) (convert callees)) m
			convert fs = Set.fromList (map symbol fs)

extractErrorCodes :: String -> TErrorCodes
extractErrorCodes text = map extract $ every_second $ lines text
	where
		every_second [] = []
		every_second (_:[]) = []
		every_second (_:x:r) = x : every_second r
		extract line = read $ takeWhile (/=')') $ tail $ dropWhile (/='(') line
