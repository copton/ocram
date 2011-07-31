module Ocram.Test.Tests.Filter.Constraints (
	tests
) where

import Ocram.Types
import Ocram.Test.Lib
import Ocram.Analysis.Constraints (check_constraints)
import Ocram.Test.Tests.Filter.Utils (extractErrorCodes)
import Control.Monad.Reader (ask)
import Data.Set (fromList)

tests = runTests "Constraints" reduce $ threads ++ function_pointer

reduce :: (String, [String], [String]) -> ER [Int]
reduce (code, cf, sr)  = do
	let ast = parse code
	let cf' = fromList cf
	let sr' = fromList sr
	opt <- ask
	return $ case execER opt (check_constraints cf' sr' ast) of
		Left e -> extractErrorCodes e
		Right _ -> []

threads = [
	(("", [], []), [2]),
	(("__attribute__((tc_blocking)) void foo();",
		["foo"], []), [2]),
	(([$paste|
		__attribute__((tc_blocking)) void foo(); 
	  __attribute__((tc_run_thread)) void baz() { foo(); }
	|], ["foo", "baz"], ["baz"]), [])
	]

function_pointer = [
	(([$paste|
		__attribute__((tc_blocking)) void foo(); 
		void bar(void*); 
		__attribute__((tc_run_thread)) void baz() { 
			bar(&foo); 
			foo();
		}
	|], ["foo", "baz"], ["baz"]), [1])
	]
