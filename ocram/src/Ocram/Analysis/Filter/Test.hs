module Ocram.Analysis.Filter.Test (tests) where

import Ocram.Analysis.CallGraph (call_graph)
import Ocram.Analysis.Filter (check_sanity, check_constraints, ErrorCode(..))
import Ocram.Test.Lib (enrich, enumTestGroup, paste)
import Ocram.Text (OcramError(errCode))
import Test.Framework (testGroup, Test)
import Test.HUnit ((@=?))

tests :: Test -- {{{1
tests = testGroup "Filter" [test_check_sanity, test_check_constraints]

test_check_sanity :: Test -- {{{1
test_check_sanity = enumTestGroup "check_sanity" $ map runTest [
    -- function declaration without parameter name {{{2
    ([paste|
      __attribute__((tc_blocking)) void block(int);
      __attribute__((tc_run_thread)) void start() {
        block(23);
      }
    |], [NoVarName])
  , -- function definition without parameter {{{2
    ("void f { }", [NoParameterList])
  , -- function definition without return type {{{2
    ("__attribute__((foo)) f() { }", [NoReturnType])
  , -- function declaration without return type {{{2
    ("__attribute__((foo)) f();", [NoReturnType])
  , -- nested functions {{{2
    ("void foo() {void bar() { } bar();}", [NestedFunction])
  , -- case ranges {{{2
    ([paste|
      int foo(int i) {
        switch (i) {
          case 1 ... 10: return 23;
          default: return 42;
        }
      }
    |], [CaseRange])
  ]
  where
    runTest (code, expected) = expected @=? errs (enrich code)
    errs ast = case check_sanity ast of
      Left es -> enrich $ map errCode es
      Right _ -> []

test_check_constraints :: Test -- {{{1
test_check_constraints = enumTestGroup "check_constraints" $ map runTest [
    -- no threads {{{2
    ("", [NoThreads])
  , ("void foo();", [NoThreads])
  , ("void foo() { };", [NoThreads])
  , ("__attribute__((tc_run_thread)) void start() { };", [ThreadNotBlocking, NoThreads])
  , -- Ellipses {{{2
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
  , -- pointer to critical function {{{2
    ([paste|
      __attribute__((tc_blocking)) void block();
      void f(void*);
      __attribute__((tc_run_thread)) void start() {
        f(&block);
        block();
      }
    |], [PointerToCriticalFunction])
  , -- cyclic call graph {{{2
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
  , -- initializer lists not supported yet {{{2
    ([paste|
      struct Foo { int i; };
      __attribute__((tc_blocking)) void block();
      __attribute__((tc_run_thread)) void start() {
        struct Foo foo = {23};
        int i[] = {4,2};
        block();
      }
    |], [InitializerList, InitializerList])
  , -- but initializer lists outside of critical functions are okay {{{2
    ([paste|
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
    |], [])
  ]
  where
    runTest (code, expected) = expected @=? errs (enrich code)
    errs ast = case check_constraints ast (call_graph ast) of
        Left es -> enrich $ map errCode es
        Right _ -> []
