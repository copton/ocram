{-# LANGUAGE QuasiQuotes #-}
module Ocram.Analysis.Filter.Test (tests) where

-- imports {{{1
import Ocram.Analysis.CallGraph (call_graph)
import Ocram.Analysis.Filter (global_constraints, critical_constraints, ErrorCode(..))
import Ocram.Test.Lib (enrich, enumTestGroup, paste)
import Ocram.Text (OcramError(errCode))
import Test.Framework (testGroup, Test)
import Test.HUnit ((@=?), Assertion)

tests :: Test -- {{{1
tests = testGroup "Filter" [test_global_constraints, test_critical_constraints]

test_global_constraints :: Test -- {{{1
test_global_constraints = enumTestGroup "global_constraints" $ map runTest [
    -- , 01 nested functions not supported {{{2
    ([paste|
      void bar() {
        void foo() { }
        foo();
      }
    |], [NestedFunction])
  , -- 02 nested functions not supported {{{2
    ([paste|
      __attribute__((tc_blocking)) void block();
      __attribute__((tc_run_thread)) void start() {
        void foo() { }
        block();
        foo();
      }
    |], [NestedFunction])
  ]
  where
    runTest (code, expected) = expected @=? errs (enrich code)
    errs ast = case global_constraints ast of
      Left es -> enrich $ map errCode es
      Right _ -> []

test_critical_constraints :: Test -- {{{1
test_critical_constraints = enumTestGroup "critical_constraints" $ map runTest [
    -- , 01 - no threads {{{2
    ("", [NoThreads])
  , ("void foo();", [NoThreads])
  , ("void foo() { };", [NoThreads])
  , ("__attribute__((tc_run_thread)) void start() { };", [ThreadNotBlocking])
  , -- 05 - Ellipses {{{2
    ([paste|
       __attribute__((tc_blocking)) void block(int x, ...);
       void non_critical(int x, ...) { }
       void critical(int x, ...) {
         block(x);
         non_critical(x);
       } 
       __attribute__((tc_run_thread)) void start() {
          critical(23);
       }
  |], [Ellipses, Ellipses])
  , -- 06 - pointer to critical function {{{2
    ([paste|
      __attribute__((tc_blocking)) void block();
      void f(void*);
      __attribute__((tc_run_thread)) void start() {
        f(&block);
        block();
      }
    |], [PointerToCriticalFunction])
  , -- 07 - cyclic call graph {{{2
    ([paste|
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
    |], [CriticalRecursion])
  , -- 08 - initializer lists not supported yet {{{2
    ([paste|
      struct Foo { int i; };
      __attribute__((tc_blocking)) void block();
      __attribute__((tc_run_thread)) void start() {
        struct Foo foo = {23};
        int i[] = {4,2};
        block();
      }
    |], [InitializerList, InitializerList])
  , -- 09 - inline assembler not supported {{{2
    ([paste|
      __attribute__((tc_blocking)) void block();
      __attribute__((tc_run_thread)) void start() {
        asm("movl %ebx, %eax");
        block();
      }
    |], [AssemblerCode])
  , -- 10 - case ranges not supported {{{2
    ([paste|
      __attribute__((tc_blocking)) void block();
      __attribute__((tc_run_thread)) void start() {
        int i;
        switch (i) {
          case 1 ... 2: block();
        }
      }
    |], [CaseRange])
  , -- 11 - statement expressions not supported {{{2
    ([paste|
      __attribute__((tc_blocking)) void block();
      __attribute__((tc_run_thread)) void start() {
          int a, b;
          int i = ( {int _a = (a), _b = (b); _a > _b ? _a : _b; } );
          block();
      }
    |], [StatExpression])
  , -- 12 - computed gotos not supported {{{2
    ([paste|
      __attribute__((tc_blocking)) void block();
      void crit(void* x) {
        goto *x;
        block();
      }
      __attribute__((tc_run_thread)) void start() {
        crit((void*)23);
      }
    |], [GotoPtr])
  , -- 13 - array range designators not supported {{{2
    ([paste|
      __attribute__((tc_blocking)) void block();
      __attribute__((tc_run_thread)) void start() {
        int widths[] = { [0 ... 9] = 1, [10] = 3 };
        block();
      }
    |], [InitializerList, RangeDesignator])
  , -- 14 - old-style parameter declaration {{{2
    ([paste|
      __attribute__((tc_blocking)) void block();

      void c(i, j)
      int i;
      char j;
      {
        block();
      }

      __attribute__((tc_run_thread)) void start() {
        c(1, 'a');
      }
    |], [OldStyleParams])
  , -- 15 - switch without body {{{2
    ([paste|
      __attribute__((tc_blocking)) void block();
      __attribute__((tc_run_thread)) void start() {
        switch (i) ;
        block();
      }
    |], [IllFormedSwitch])
  , -- 16 - switch with preceding statement {{{2
    ([paste|
      __attribute__((tc_blocking)) void block();
      __attribute__((tc_run_thread)) void start() {
        switch (i) {
          block();
          case 1: return;
        }
      }
    |], [IllFormedSwitch])
  , -- 17 - switch with preceding declaration {{{2
    ([paste|
      __attribute__((tc_blocking)) void block();
      __attribute__((tc_run_thread)) void start() {
        switch (i) {
          int j;
          case 1: block();
        }
      }
    |], [IllFormedSwitch])
  , -- 18 - switch with multiple default statements {{{2
    ([paste|
      __attribute__((tc_blocking)) void block();
      __attribute__((tc_run_thread)) void start() {
        switch (i) {
          case 0: block();
          default: ;
          default: ;
        }
      }
    |], [IllFormedSwitch])
  , -- 19 - switch with case after default statement {{{2
    ([paste|
      __attribute__((tc_blocking)) void block();
      __attribute__((tc_run_thread)) void start() {
        switch (i) {
          case 0: ;
          default: ;
          case 1: block();
        }
      }
    |], [IllFormedSwitch])
  , -- 20 - blocking function declaration without parameter name {{{2
    ([paste|
      __attribute__((tc_blocking)) void block(int);
      __attribute__((tc_run_thread)) void start() {
        block(23);
      }
    |], [NoVarName])
  , -- 21 - function definition without parameter {{{2
    ([paste|
      __attribute__((tc_blocking)) void block(int i);
      int c {
        block(23);
      }
      __attribute__((tc_run_thread)) void start() {
        c(23);
      }
    |], [NoParameterList])
  , -- 22 - function declaration without return type {{{2
    ([paste|
      __attribute__((tc_blocking)) block(int i);
      __attribute__((tc_run_thread)) void start() {
        block(23);
      }
    |], [NoReturnType])
  , -- 23 - function definition without return type {{{2
    ([paste|
      __attribute__((tc_blocking)) block(int i);

      c(int i) {
        block(i+1);
      }

      __attribute__((tc_run_thread)) void start() {
        c(23);
      }
    |], [NoReturnType, NoReturnType])
  -- end {{{2
  ]
  where
    runTest :: (String, [ErrorCode]) -> Assertion -- {{{2
    runTest (code, expected) = expected @=? errs (enrich code)

    errs ast = case critical_constraints ast (call_graph ast) of
        Left es -> enrich $ map errCode es
        Right _ -> []
