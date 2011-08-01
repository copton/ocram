module Ocram.Test.Tests.Analysis.Constraints (
	tests
) where

import Ocram.Types
import Ocram.Test.Tests.Analysis.Types
import Ocram.Test.Tests.Analysis.Utils
import Ocram.Analysis.Constraints (check_constraints)
import Control.Monad.Reader (ask)

type Input = (TCriticalFunctions, TStartRoutines)
type Output = TErrorCodes

reduce :: Ast -> Input -> ER Output
reduce ast (cf, sr) = do
	opt <- ask
	let check = check_constraints (enrich_cf cf) (enrich_sr sr) ast
	return $ case execER opt check of
		Left e -> extractErrorCodes e
		Right _ -> []

setup :: TCase -> (Input, Output)
setup tc = ((tcCriticalFunctions tc, tcStartRoutines tc), tcConstraints tc)

tests = runTests "Constraints" reduce setup

--threads = [
--	(("", [], []), [2]),
--	(("__attribute__((tc_blocking)) void foo();",
--		["foo"], []), [2]),
--	(([$paste|
--		__attribute__((tc_blocking)) void foo(); 
--	  __attribute__((tc_run_thread)) void baz() { foo(); }
--	|], ["foo", "baz"], ["baz"]), [])
--	]
--
--function_pointer = [
--	(([$paste|
--		__attribute__((tc_blocking)) void foo(); 
--		void bar(void*); 
--		__attribute__((tc_run_thread)) void baz() { 
--			bar(&foo); 
--			foo();
--		}
--	|], ["foo", "baz"], ["baz"]), [1])
--	]
