module Ocram.Test.Tests.Analysis.TestCases (
	test_cases
) where

import Ocram.Test.Lib (paste)
import Ocram.Options (defaultOptions)
import Ocram.Test.Tests.Analysis.Types

--data TCase = TCase {
--	tcCode :: TCode,
--	tcOptions :: Options,
--	tcDefinedFunctions :: TDefinedFunctions,
--	tcBlockingFunctions :: TBlockingFunctions,
--	tcStartRoutines :: TStartRoutines,
--	tcCriticalFunctions :: TCriticalFunctions,
--	tcCallGraph :: TCallGraph,
--	tcSanity :: TErrorCodes,
--	tcADG :: TErrorCodes,
--	tcConstraints :: TErrorCodes
--}	

test_cases :: [TCase]
test_cases = [
-- empty code {{{2
	 TCase 
		""
		defaultOptions
		[]
		[]
		[]
		[]
		[]
		[]
		[]
		[2]
-- single function declaration {{{2
	,TCase
		"void foo();"
		defaultOptions
		[]
		[]
		[]
		[]
		[]
		[]		
		[]
		[2]
-- single function definition {{{2
	,TCase
		"void foo() { }"
		defaultOptions
		["foo"]
		[]
		[]
		[]
		[]
		[]
		[]
		[2]
-- single blocking function declaration, 1 {{{2
	,TCase
		"__attribute__((tc_blocking)) void foo();"
		defaultOptions
		[]
		["foo"]
		[]
		["foo"]
		[]
		[]
		[]
		[2]
-- single blocking function declaration, 2 {{{2
	,TCase
		"__attribute__((tc_blocking)) int foo(char);"
		defaultOptions
		[]
		["foo"]
		[]
		["foo"]
		[]
		[]
		[]
		[2]
-- single blocking function declaration, 3 {{{2
	,TCase
		"__attribute__((tc_blocking)) void foo(int x, ...);"
		defaultOptions
		[]
		["foo"]
		[]
		["foo"]
		[]
		[]
		[]
		[2]
-- minimal thread {{{2
	,TCase
		[$paste|
			__attribute__((tc_blocking)) void block();
			__attribute__((tc_run_thread)) void start() {
				block();
			}
		|]
		defaultOptions
		["start"]
		["block"]
		["start"]
		["block", "start"]
		[("block", ["start"], []), ("start", [], ["block"])]
		[]
		[]
		[]
-- finish {{{2
	]
