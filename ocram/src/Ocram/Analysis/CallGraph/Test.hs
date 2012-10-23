{-# LANGUAGE QuasiQuotes #-}
module Ocram.Analysis.CallGraph.Test (tests) where

-- imports {{{1
import Data.Maybe (isJust, fromJust)
import Ocram.Analysis.CallGraph (call_graph, call_chain, call_order, get_callees, get_callers)
import Ocram.Test.Lib (enrich, reduce, enumTestGroup, paste)
import Test.Framework (testGroup, Test)
import Test.HUnit ((@=?), (@?))

tests :: Test -- {{{1
tests = testGroup "CallGraph" [
    test_call_graph
  , test_call_chain
  , test_call_order
  , test_get_callees
  , test_get_callers
  ]

test_call_graph :: Test -- {{{1
test_call_graph = enumTestGroup "call_graph" $ map runTest [
    -- minimal example {{{2
    ([paste|
      __attribute__((tc_blocking)) void block();
      __attribute__((tc_run_thread)) void start() {
        block();
      }
    |], [("start", "block")])
  ,  -- with critical function {{{2
    ([paste|
      __attribute__((tc_blocking)) void block();
      void critical() {
        block();
      }
      __attribute__((tc_run_thread)) void start() {
        critical();
      }
    |], [("critical", "block"), ("start", "critical")])
  , -- chain of critical functions {{{2
    ([paste|
      __attribute__((tc_blocking)) void block();
      void c1() { c2(); }
      void c2() { c3(); }
      void c3() { c4(); }
      void c4() { block(); }
      __attribute__((tc_run_thread)) void start() {
        c1();
      }
    |], [("c1", "c2"), ("c2", "c3"), ("c3", "c4"), ("c4", "block"), ("start", "c1")])
  , -- additional non-critical function {{{2
    ([paste|
      __attribute__((tc_blocking)) void block();
      void non_critical() { }
      __attribute__((tc_run_thread)) void start() {
        non_critical();
        block();
      }
    |], [("start", "block")])
  , -- ignore unused blocking functions {{{2
    ([paste|
      __attribute__((tc_blocking)) void block_unused();
      __attribute__((tc_blocking)) void block_used();
      __attribute__((tc_run_thread)) void start () { block_used(); }
    |], [("start", "block_used")])
  , -- call of functions from external libraries {{{2
    ([paste|
      __attribute__((tc_blocking)) void block();
      int libfun();
      __attribute__((tc_run_thread)) void start() {
        libfun();
        block();
      }
    |], [("start", "block")])
  , -- two independant threads {{{2
    ([paste|
      __attribute__((tc_blocking)) void block1();
      __attribute__((tc_blocking)) void block2();
      __attribute__((tc_run_thread)) void start1() {
        block1();
      }
      __attribute__((tc_run_thread)) void start2() {
        block2();
      }
    |], [("start1", "block1"), ("start2", "block2")])
  , -- reentrance {{{2
    ([paste|
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
    |], [("critical", "block"), ("start1", "critical"), ("start2", "critical")])
  ]
  where runTest (code, expected) = expected @=? reduce (call_graph (enrich code))

test_call_chain :: Test -- {{{1
test_call_chain = enumTestGroup "call_chain" $ map runTest [
    ([("a", "b")], ("a", "a"), ["a"])
  , ([("a", "b")], ("a", "b"), ["a", "b"])
  , ([("a", "b"), ("a", "c")], ("a", "b"), ["a", "b"])
  , ([("a", "b"), ("a", "c")], ("a", "c"), ["a", "c"])
  ]
  where
    runTest (cg, (start, end), expected) = do
      let result = call_chain (enrich cg) (enrich start) (enrich end)
      isJust result @? "could not determine call chain."
      expected @=? (reduce $ fromJust result)

test_call_order :: Test -- {{{1
test_call_order = enumTestGroup "call_order" $ map runTest [
    ([("a", "b")], "a", ["a", "b"])
  , ([("a", "b"), ("b", "c")], "a", ["a", "b", "c"])
  , ([("a", "b"), ("a", "c")], "a", ["a", "b", "c"])
  ]
  where
    runTest (cg, start, expected) = do
      let result = call_order (enrich cg) (enrich start)
      isJust result @? "could not determine call order"
      expected @=? (reduce $ fromJust result)

test_get_callees :: Test -- {{{1
test_get_callees = enumTestGroup "get_callees" $ map runTest [
    ([("a", "b")], "a", ["b"])
  , ([("a", "b")], "b", [])
  , ([("a", "b"), ("b", "c")], "b", ["c"])
  , ([("a", "c"), ("a", "b")], "a", ["c", "b"])
  ]
  where
    runTest (cg, function, expected) = do
      let result = get_callees (enrich cg) (enrich function)
      expected @=? (map fst result)

test_get_callers :: Test -- {{{1
test_get_callers = enumTestGroup "get_callers" $ map runTest [
    ([("a", "b")], "b", ["a"])
  , ([("a", "b")], "a", [])
  , ([("a", "b"), ("b", "c")], "b", ["a"])
  , ([("a", "b"), ("c", "b")], "b", ["a", "c"])
  ]
  where
    runTest (cg, function, expected) = do
      let result = get_callers (enrich cg) (enrich function)
      expected @=? (map fst result)
