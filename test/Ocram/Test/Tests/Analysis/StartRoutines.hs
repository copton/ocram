module Ocram.Test.Tests.Analysis.StartRoutines (
	tests
) where 

import Ocram.Types
import Ocram.Test.Lib
import Ocram.Analysis.StartRoutines
import Ocram.Symbols (symbol)
import qualified Data.Set as Set

reduce :: (String, DefinedFunctions) -> ER (Set.Set String)
reduce (code, df) = do
	let ast = parse code
	sr <- start_routines df ast
	return $ (Set.map symbol) sr

tests = runTests "StartRoutines" reduce
	[ (
			(
				"__attribute__((tc_run_thread)) void foo() { }", 
				Set.singleton "foo"
			),
			Set.singleton "foo"
		),
	  (
			(
				"void __attribute__((tc_run_thread)) foo() { }", 
				Set.singleton "foo"
			),
			Set.singleton "foo"
		),
	 (("void foo() { }", Set.singleton "foo"), Set.empty),
	 (("", Set.empty), Set.empty),
	 (("void foo() {}", Set.empty), Set.empty)
	]
