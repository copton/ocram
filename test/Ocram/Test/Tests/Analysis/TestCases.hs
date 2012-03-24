module Ocram.Test.Tests.Analysis.TestCases 
-- export {{{1
(
	test_cases
) where

-- import {{{1
import Ocram.Analysis.Filter (ErrorCode(..))
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
		[NoThreads]
		[]
-- single function declaration {{{2
	,TCase
		"void foo();"
		[]
		[]		
		[NoThreads]
		[]
-- single function definition {{{2
	,TCase
		"void foo() { }"
		[]
		[]
		[NoThreads]
		[]
-- single start function {{{2
	,TCase
		"__attribute__((tc_run_thread)) void start() { };"
		[]
		[]
		[ThreadNotBlocking, NoThreads]
		[]
-- minimal thread {{{2
	,TCase
		[paste|
			__attribute__((tc_blocking)) void block();
			__attribute__((tc_run_thread)) void start() {
				block();
			}
		|]
		[("start", "block")]
		[]
		[]
		[]
-- minimal thread - complex blocking declratation{{{2
	,TCase
		[paste|
			__attribute__((tc_blocking)) void* block(struct hemmerned* foo);
			__attribute__((tc_run_thread)) void start() {
				block(0);
			}
		|]
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
		[("c1", "c2"), ("c2", "c3"), ("c3", "c4"), ("c4", "block"), ("start", "c1")]
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
		[("start", "block")]
		[]
		[]
		[]
-- only regard actually used blocking functions {{{2
  ,TCase
    [paste|
      __attribute__((tc_blocking)) void block_unused();
      __attribute__((tc_blocking)) void block_used();
      __attribute__((tc_run_thread)) void start () { block_used(); }
    |]
    [("start", "block_used")]
    []
    []
    []
-- call of functions from external libraries {{{2
  ,TCase
    [paste|
      __attribute__((tc_blocking)) void block();
      int libfun();
      __attribute__((tc_run_thread)) void start() {
        libfun();
        block();
      }
    |]
    [("start", "block")]
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
		[("start", "block")]
		[]
		[PointerToCriticalFunction]
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
		[("c1", "c2"), ("c2", "block"), ("c2", "c1"), ("start", "c1")]
		[]
		[CriticalRecursion]
		[]
-- function definition without parameter {{{2
	,TCase
		"void f { }"
		undefined 
		[NoParameterList] 
		undefined
		[TTCallGraph, TTConstraints]
-- function definition without return type {{{2
	,TCase
		"__attribute__((foo)) f() { }"
		undefined 
		[NoReturnType] 
		undefined
		[TTCallGraph, TTConstraints]
-- function declaration without return type {{{2
	,TCase
		"__attribute__((foo)) f();"
		undefined 
		[NoReturnType] 
		undefined
		[TTCallGraph, TTConstraints]
-- initializer lists not supported yet {{{2
  ,TCase
    [paste|
      struct Foo { int i; };
      __attribute__((tc_blocking)) void block();
      __attribute__((tc_run_thread)) void start() {
        struct Foo foo = {23};
        int i[] = {4,2};
        block();
      }
    |]
    undefined
    undefined
    [InitializerList, InitializerList]
		[TTCallGraph, TTSanity]
-- but initializer lists outside of critical functions are okay {{{2
  ,TCase
    [paste|
      struct Foo { int i; };
      const char text[] = "it's okay";
      __attribute__((tc_blocking)) void block();
      void foo() {
        struct Foo foo = {23};
        int i[] = {4,2};
      }
      __attribute__((tc_run_thread)) void start() {
        block();
        foo();
      }
    |]
    undefined
    undefined
    []
		[TTCallGraph, TTSanity]
-- name-less parameters of blocking functions {{{2
  ,TCase
    [paste|
      __attribute__((tc_blocking)) void block(int);
      __attribute__((tc_run_thread)) void start() {
        block(23);
      }
    |]
    undefined
    [NoVarName]
    undefined
    [TTCallGraph, TTConstraints] 
-- no ellipses for critical functions {{{2
   ,TCase
    [paste|
     __attribute__((tc_blocking)) void block(int x, ...);
     void non_critical(int x, ...) { }
     void critical(int x, ...) {
       block(x);
       non_critical(x);
     } 
     __attribute__((tc_run_thread)) void start() {
        critical(23);
     }
    |]
     undefined
     []
     [Ellipses, Ellipses]
     [TTCallGraph]
	]
