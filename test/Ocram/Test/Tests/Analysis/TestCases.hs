module Ocram.Test.Tests.Analysis.TestCases 
-- export {{{1
(
	test_cases
) where

-- import {{{1
import Ocram.Test.Lib (paste)
import Ocram.Options (defaultOptions)
import Ocram.Test.Tests.Analysis.Types

-- test_cases :: [TCase] {{{1
test_cases :: [TCase]
test_cases = [
--	Code
--	Options
--	DefinedFunctions
--	BlockingFunctions
--	StartRoutines
--	CriticalFunctions
--	CallGraph
--	Sanity
--	ADG
--	Constraints
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
		[]
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
		[]
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
		[]
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
		[]
-- single blocking function declaration, 2 {{{2
	,TCase
		"int __attribute__((tc_blocking)) foo(char);"
		defaultOptions
		[]
		["foo"]
		[]
		["foo"]
		[]
		[]
		[]
		[2]
		[]
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
		[]
-- minimal thread, 1 {{{2
	,TCase
		[paste|
			__attribute__((tc_blocking)) void block();
			void __attribute__((tc_run_thread)) start() {
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
		[]
-- minimal thread, 2 {{{2
	,TCase
		[paste|
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
		[]
-- additional critical function {{{2
	,TCase
		[paste|
			__attribute__((tc_blocking)) void block();
			void critical() {
				block();
			}
			__attribute__((tc_run_thread)) void start() {
				critical();
			}
		|]
		defaultOptions
		["critical", "start"]
		["block"]
		["start"]
		["block", "critical", "start"]
		[("block", ["critical"], []), ("critical", ["start"], ["block"]), ("start", [], ["critical"])]
		[]
		[]
		[]
		[]
-- chain of critical functions {{{2
	,TCase
		[paste|
			__attribute__((tc_blocking)) void block();
			void c1() { c2(); }
			void c2() { c3(); }
			void c3() { c4(); }
			void c4() { block(); }
			__attribute__((tc_run_thread)) void start() {
				c1();
			}
		|]
		defaultOptions
		["c1", "c2", "c3", "c4", "start"]
		["block"]
		["start"]
		["block", "c1", "c2", "c3", "c4", "start"]
		[
			("block",["c4"],[]),
			("c1",["start"],["c2"]),
			("c2",["c1"],["c3"]),
			("c3",["c2"],["c4"]),
			("c4",["c3"],["block"]),
			("start",[],["c1"])
		]
		[]
		[]
		[]
		[]
-- additional non-critical function {{{2
	,TCase
		[paste|
			__attribute__((tc_blocking)) void block();
			void non_critical() { }
			__attribute__((tc_run_thread)) void start() {
				non_critical();
				block();
			}
		|]
		defaultOptions
		["non_critical", "start"]
		["block"]
		["start"]
		["block", "start"]
		[("block", ["start"], []), ("non_critical", ["start"], []), ("start", [], ["block","non_critical"])]
		[]
		[]
		[]
		[]
-- two independant threads {{{2
	,TCase
		[paste|
			__attribute__((tc_blocking)) void block1();
			__attribute__((tc_blocking)) void block2();
			__attribute__((tc_run_thread)) void start1() {
				block1();
			}
			__attribute__((tc_run_thread)) void start2() {
				block2();
			}
		|]
		defaultOptions
		["start1", "start2"]
		["block1", "block2"]
		["start1", "start2"]
		["block1", "block2", "start1", "start2"]
		[
			("block1", ["start1"], []),
			("block2", ["start2"], []),
			("start1", [], ["block1"]),
			("start2", [], ["block2"])
		]
		[]
		[]
		[]
		[]
-- reentrance {{{2
	,TCase
		[paste|
			__attribute__((tc_blocking)) void block();
			void critical() {
				block();
			}
			__attribute__((tc_run_thread)) void start1() {
				critical();
			}
			__attribute__((tc_run_thread)) void start2() {
				critical();
			}
		|]
		defaultOptions
		["critical", "start1", "start2"]
		["block"]
		["start1", "start2"]
		["block", "critical", "start1", "start2"]
		[
			("block", ["critical"], []),
			("critical", ["start1", "start2"], ["block"]),
			("start1", [], ["critical"]),
			("start2", [], ["critical"])
		]
		[]
		[]
		[]
		[]
-- pointer to critical function {{{2
	,TCase
		[paste|
			__attribute__((tc_blocking)) void block();
			void f(void*);
			__attribute__((tc_run_thread)) void start() {
				f(&block);
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
		[1]
		[]
-- cyclic call graph {{{2
	,TCase
		[paste|
			__attribute__((tc_blocking)) void block();
			void c2() {
				block();
				c1();
			}

			void c1() {
				c2();
			}
		
			__attribute__((tc_run_thread)) void start() {
				c1();
			}
		|]
		defaultOptions
		["c1", "c2", "start"]
		["block"]
		["start"]
		undefined
		[
			("block", ["c2"], []),
			("c1", ["c2", "start"], ["c2"]),
			("c2", ["c1"], ["block", "c1"]),
			("start", [], ["c1"])
		]
		[]
		[1]
		[]
		[TTCriticalFunctions]
-- function definition without parameter {{{2
	,TCase
		"void f { }"
		defaultOptions
		undefined undefined undefined undefined undefined 
		[1] 
		undefined undefined
		[TTDefinedFunctions, TTBlockingFunctions, TTStartRoutines, TTCriticalFunctions, TTCallGraph, TTADG, TTConstraints]
-- finish {{{2
	]
