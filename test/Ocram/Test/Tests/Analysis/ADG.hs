module Ocram.Test.Tests.Filter.CallGraph (
	tests
) where

import Ocram.Types
import Ocram.Test.Lib 
import Ocram.Test.Tests.Filter.Utils (extractErrorCodes)
import Ocram.Symbols (symbol)
import Ocram.Analysis.ADG (check_call_graph)
import Data.Set (fromList)
import Control.Monad.Reader (ask)
import qualified Data.Map as Map
import qualified Data.Set as Set

type CG = [(String, [String], [String])]

constructCallGraph :: CG -> CallGraph
constructCallGraph cg = foldl construct Map.empty cg
	where
		construct m (function, callers, callees) =
			Map.insert (symbol function) (Entry (convert callers) (convert callees)) m
		convert fs = Set.fromList (map symbol fs)

reduce :: (String, CG, [String], [String]) -> ER [Int]
reduce (code, cg, sr, df) = do
	let ast = parse code
	let sr'= Set.fromList sr
	let df' = Set.fromList df
	let cg' = constructCallGraph cg
	opt <- ask
	return $ case execER opt (check_call_graph cg' sr' df' ast) of
		Left e -> extractErrorCodes e
		Right _ -> []

tests = runTests "CallGraph" reduce $ map createTestCase test_cases

createTestCase

--tests = runTests "CallGraph" reduce [
--	(([$paste|
--			void foo() {
--				bar();
--			} 
--
--			void bar() {
--				foo();
--			}
--		|],
--		[("foo", ["bar"], ["bar"]), ("bar", ["foo"], ["foo"])],
--		[""],
--		["foo", "bar"]),
--		[]),
--	(([$paste|
--			__attribute__((tc_blocking)) void block(); 
--			__attribute__((tc_run_thread)) void start() {
--				rec();
--			} 
--			void rec() {
--				block(); 
--				start();
--			}
--		|], 
--		[("block", ["rec"], []), ("start", ["rec"], ["rec"]), ("rec", ["start"], ["block", "start"])],
--		["start"],
--		["start", "rec"]),
--		[1]),
--	(([$paste|
--			__attribute__((tc_blocking)) int block(int i);
--
--			__attribute__((tc_run_thread)) void start() 
--			{
--				int i;
--				i = block(i);
--			}
--		|], 
--		[("block", ["start"], []), ("start", [], ["block"])],
--		["start"],
--		["start"]),
--		[])
--	]
