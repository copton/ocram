module Ocram.Test.Tests.Analysis.ADG (
	tests
) where

import Ocram.Types
import Ocram.Test.Tests.Analysis.Types
import Ocram.Test.Tests.Analysis.Utils
import Ocram.Analysis.ADG (check_call_graph)
import Control.Monad.Reader (ask)

type Input = (TCallGraph, TStartRoutines, TDefinedFunctions)
type Output = TErrorCodes

execute :: Ast -> Input -> ER Output
execute ast (cg, sr, df) = do
	opt <- ask
	let check = check_call_graph (enrich cg) (enrich sr) (enrich df) ast
	return $ case execER opt check of
		Left e -> extractErrorCodes e
		Right _ -> []

setup :: TCase -> (Input, Output)
setup tc = ((tcCallGraph tc, tcStartRoutines tc, tcDefinedFunctions tc), tcADG tc)

tests = runTests "CallGraph" execute setup

--tests = runTests "CallGraph" execute [
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
