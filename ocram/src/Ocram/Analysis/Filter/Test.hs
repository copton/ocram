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
      __attribute__((tc_block)) void block();
      __attribute__((tc_thread)) void start() {
        void foo() { }
        block();
        foo();
      }
    |], [NestedFunction])
  , -- 03 thread local storage {{{2
    ([paste|
      __thread int i;

      __attribute__((tc_block)) void block(int i);
      __attribute__((tc_thread)) void start() {
        block(i);
      }
    |], [ThreadLocalStorage])
  , -- 04 - main function definition {{{2
    ([paste|
      void main() { }

      __attribute__((tc_block)) void block(int i);
      __attribute__((tc_thread)) void start() {
        block(i);
      }
    |], [MainFunction])
  , -- 05 - main function declaration {{{2
    ([paste|
      void main();

      __attribute__((tc_block)) void block(int i);
      __attribute__((tc_thread)) void start() {
        block(i);
      }
    |], [MainFunction])
  , -- 06 - reserved prefix {{{2
    ([paste|
      void ec_foo();
      void ec_bar() { }
      int ec_j;
      __attribute__((tc_block)) void ec_blcok(int ec_i);
      __attribute__((tc_thread)) void ec_start() {
        block(ec_j);
      }
    |], replicate 7 ReservedPrefix)
  -- end {{{2
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
  , ("__attribute__((tc_thread)) void start() { };", [ThreadNotBlocking])
  , -- 05 - Ellipses {{{2
    ([paste|
       __attribute__((tc_block)) void block(int x, ...);
       void non_critical(int x, ...) { }
       void critical(int x, ...) {
         block(x);
         non_critical(x);
       } 
       __attribute__((tc_thread)) void start() {
          critical(23);
       }
  |], [Ellipses, Ellipses])
  , -- 07 - cyclic call graph {{{2
    ([paste|
      __attribute__((tc_block)) void block();
      void c2() {
        block();
        c1();
      }
      void c1() {
        c2();
      }
      __attribute__((tc_thread)) void start() {
        c1();
      }
    |], [CriticalRecursion])
  , -- 08 - initializer lists for arrays {{{2
    ([paste|
      struct Foo { int i; };
      __attribute__((tc_block)) void block();
      __attribute__((tc_thread)) void start() {
        struct Foo foo = {23};
        int i[] = {4,2};
        block();
      }
    |], [ArrayInitializerList])
  , -- 09 - inline assembler not supported {{{2
    ([paste|
      __attribute__((tc_block)) void block();
      __attribute__((tc_thread)) void start() {
        asm("movl %ebx, %eax");
        block();
      }
    |], [AssemblerCode])
  , -- 10 - case ranges not supported {{{2
    ([paste|
      __attribute__((tc_block)) void block();
      __attribute__((tc_thread)) void start() {
        int i;
        switch (i) {
          case 1 ... 2: block();
        }
      }
    |], [CaseRange])
  , -- 11 - statement expressions not supported {{{2
    ([paste|
      __attribute__((tc_block)) void block();
      __attribute__((tc_thread)) void start() {
          int a, b;
          int i = ( {int _a = (a), _b = (b); _a > _b ? _a : _b; } );
          block();
      }
    |], [StatExpression])
  , -- 12 - computed gotos not supported {{{2
    ([paste|
      __attribute__((tc_block)) void block();
      void crit(void* x) {
        goto *x;
        block();
      }
      __attribute__((tc_thread)) void start() {
        crit((void*)23);
      }
    |], [GotoPtr])
  , -- 13 - array range designators not supported {{{2
    ([paste|
      __attribute__((tc_block)) void block();
      __attribute__((tc_thread)) void start() {
        int widths[] = { [0 ... 9] = 1, [10] = 3 };
        block();
      }
    |], [ArrayInitializerList, RangeDesignator])
  , -- 14 - old-style parameter declaration {{{2
    ([paste|
      __attribute__((tc_block)) void block();

      void c(i, j)
      int i;
      char j;
      {
        block();
      }

      __attribute__((tc_thread)) void start() {
        c(1, 'a');
      }
    |], [OldStyleParams])
  , -- 15 - switch without body {{{2
    ([paste|
      __attribute__((tc_block)) void block();
      __attribute__((tc_thread)) void start() {
        switch (i) ;
        block();
      }
    |], [IllFormedSwitch])
  , -- 16 - switch with preceding statement {{{2
    ([paste|
      __attribute__((tc_block)) void block();
      __attribute__((tc_thread)) void start() {
        switch (i) {
          block();
          case 1: return;
        }
      }
    |], [IllFormedSwitch])
  , -- 17 - switch with preceding declaration {{{2
    ([paste|
      __attribute__((tc_block)) void block();
      __attribute__((tc_thread)) void start() {
        switch (i) {
          int j;
          case 1: block();
        }
      }
    |], [IllFormedSwitch])
  , -- 18 - switch with multiple default statements {{{2
    ([paste|
      __attribute__((tc_block)) void block();
      __attribute__((tc_thread)) void start() {
        switch (i) {
          case 0: block();
          default: ;
          default: ;
        }
      }
    |], [IllFormedSwitch])
  , -- 19 - switch with case after default statement {{{2
    ([paste|
      __attribute__((tc_block)) void block();
      __attribute__((tc_thread)) void start() {
        switch (i) {
          case 0: ;
          default: ;
          case 1: block();
        }
      }
    |], [IllFormedSwitch])
  , -- 20 - blocking function declaration without parameter name {{{2
    ([paste|
      __attribute__((tc_block)) void block(int);
      __attribute__((tc_thread)) void start() {
        block(23);
      }
    |], [NoVarName])
  , -- 21 - function definition without parameter {{{2
    ([paste|
      __attribute__((tc_block)) void block(int i);
      int c {
        block(23);
      }
      __attribute__((tc_thread)) void start() {
        c(23);
      }
    |], [NoParameterList])
  , -- 22 - function declaration without return type {{{2
    ([paste|
      __attribute__((tc_block)) block(int i);
      __attribute__((tc_thread)) void start() {
        block(23);
      }
    |], [NoReturnType])
  , -- 23 - function definition without return type {{{2
    ([paste|
      __attribute__((tc_block)) block(int i);

      c(int i) {
        block(i+1);
      }

      __attribute__((tc_thread)) void start() {
        c(23);
      }
    |], [NoReturnType, NoReturnType])
  , -- 24 - non-void thread start function {{{2
    ([paste|
      __attribute__((tc_block)) void block();

      __attribute__((tc_thread)) int start() {
        block();
        return 0;
      }
    |], [StartFunctionSignature])
  , -- 25 - thread start function with parameters {{{2
    ([paste|
      __attribute__((tc_block)) void block();

      __attribute__((tc_thread)) void start(int i) {
        block();
      }
    |], [StartFunctionSignature])

  , -- 26 - function attributes {{{2
    ([paste|
      __attribute__((tc_block)) void block(int i);

      int square(int n) __attribute__((const));
      
      void c() __attribute__((noreturn));
      void c() {
        int x __attribute__ ((aligned (16))) = 23;
        block(square(x));
      }

      __attribute__((tc_thread)) void start() {
        c();
      }
    |], [GnucAttribute, GnucAttribute])
  , -- 27 - critical group {{{2
    ([paste|
      __attribute__((tc_block)) int block();

      int c(), square(int n);
      
      int c() {
        return block() + 1;
      }

      __attribute__((tc_thread)) void start() {
        c();
      }
    |], [CriticalGroup])
 
  , -- 28 - volatile qualifier {{{2
    ([paste|
      __attribute__((tc_block)) int block();

      __attribute__((tc_thread)) void start() {
        volatile int i;
        i = block();
      }
    |], [VolatileQualifier])
  -- end {{{2
  ]
  where
    runTest :: (String, [ErrorCode]) -> Assertion -- {{{2
    runTest (code, expected) = expected @=? errs (enrich code)

    errs ast = case critical_constraints (call_graph ast) ast of
        Left es -> enrich $ map errCode es
        Right _ -> []

