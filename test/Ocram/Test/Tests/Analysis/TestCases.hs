module Ocram.Test.Tests.Analysis.TestCases 
-- export {{{1
(
	test_cases
) where

-- import {{{1
import Ocram.Test.Lib (paste)
import Ocram.Test.Tests.Analysis.Types

-- test_cases :: [TCase] {{{1
test_cases :: [TCase]
test_cases = [
-- empty code {{{2
	 TCase 
		""
		[]
		[]
		[]
		[]
		[]
		[3]
		[]
-- single function declaration {{{2
	,TCase
		"void foo();"
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
		["foo"]
		[]
		["foo"]
		[]
		[]
		[2]
		[]
-- single blocking function declaration, 2 {{{2
	,TCase
		"int __attribute__((tc_blocking)) foo(char);"
		["foo"]
		[]
		["foo"]
		[]
		[]
		[2]
		[]
-- single blocking function declaration, 3 {{{2
	,TCase
		"__attribute__((tc_blocking)) void foo(int x, ...);"
		["foo"]
		[]
		["foo"]
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
		["block"]
		["start"]
		["block", "start"]
		[("start", "block")]
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
		["block"]
		["start"]
		["block", "start"]
		[("start", "block")]
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
		["block"]
		["start"]
		["block", "critical", "start"]
		[("critical", "block"), ("start", "critical")]
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
		["block"]
		["start"]
		["block", "c1", "c2", "c3", "c4", "start"]
		[("start", "c1"), ("c1", "c2"), ("c2", "c3"), ("c3", "c4"), ("c4", "block")]
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
		["block"]
		["start"]
		["block", "start"]
		[("start", "block"), ("start", "non_critical")]
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
		["block1", "block2"]
		["start1", "start2"]
		["block1", "block2", "start1", "start2"]
		[("start1", "block1"), ("start2", "block2")]
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
		["block"]
		["start1", "start2"]
		["block", "critical", "start1", "start2"]
		[("critical", "block"), ("start1", "critical"), ("start2", "critical")]
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
		["block"]
		["start"]
		["block", "start"]
		[("start", "block")]
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
		["block"]
		["start"]
		undefined
		[("c1", "c2"), ("c2", "c1"), ("start", "c1")]
		[]
		[1]
		[TTCriticalFunctions]
-- function definition without parameter {{{2
	,TCase
		"void f { }"
		undefined undefined undefined undefined 
		[1] 
		undefined
		[TTBlockingFunctions, TTStartFunctions, TTCriticalFunctions, TTCallGraph, TTConstraints]
-- finish {{{2
	]