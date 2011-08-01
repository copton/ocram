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
runTest reduce (number, (code, options, (input, output))) = TestCase $ assertEqual name output result
	where
		name = "test" ++ show number
		ast = parse code
		result = case execER options (reduce ast input) of
			Left e -> error e
			Right x -> x

runTests :: (Eq o, Show o) => String -> (Ast -> i -> ER o) -> (TCase -> (i, o)) -> Test
runTests label reduce setup = TestLabel label $ TestList $ map (runTest reduce) $ zip [1..] $ map prepare test_cases
	where
		prepare tc = (tcCode tc, tcOptions tc, setup tc)

reduce_bf :: BlockingFunctions -> TBlockingFunctions
reduce_bf = Set.toList

enrich_bf :: TBlockingFunctions -> BlockingFunctions
enrich_bf = Set.fromList

reduce_df :: DefinedFunctions -> TDefinedFunctions
reduce_df = Set.toList

enrich_df :: TDefinedFunctions -> DefinedFunctions
enrich_df = Set.fromList

reduce_sr :: StartRoutines -> TStartRoutines
reduce_sr = Set.toList

enrich_sr :: TStartRoutines -> StartRoutines
enrich_sr = Set.fromList

reduce_cf :: CriticalFunctions -> TCriticalFunctions
reduce_cf = Set.toList

enrich_cf :: TCriticalFunctions -> CriticalFunctions
enrich_cf = Set.fromList

reduce_cg :: CallGraph -> TCallGraph
reduce_cg cg = map decompose (Map.toList cg)
	where
		decompose (function, (Entry callers callees)) =
			(function, Set.toList callers, Set.toList callees)

enrich_cg :: TCallGraph -> CallGraph
enrich_cg cg = foldl construct Map.empty cg
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
