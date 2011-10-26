module Ocram.Test.Tests.Query
-- exports {{{1
(
	tests
) where

-- import {{{1 
import Ocram.Query
import Ocram.Test.Lib
import Test.HUnit
import Data.Maybe (isJust)

tests = -- {{{1
  TestLabel "Query" $ TestList $ [funDefTests, funDeclTests, blockingTests, startTests]


funDefTests = TestLabel "function_definition" $ TestList $ map runTest [
    ("void foo() { }", "foo", True)
  , ("void bar() { }", "foo", False)
  , ("void foo();", "foo", False)
  ]
  where
    runTest (code, name, expected) = TestCase $
      expected @=? (isJust $ function_definition (enrich code) (enrich name))


funDeclTests = TestLabel "function_declaration" $ TestList $ map runTest [
    ("void foo();", "foo", True)
  , ("void bar() { }", "foo", False)
  , ("void foo() { }", "foo", False)
  ]
  where
    runTest (code, name, expected) = TestCase $
      expected @=? (isJust $ function_declaration (enrich code) (enrich name))

blockingTests = TestLabel "is_blocking_function" $ TestList $ map runTest [
    ("__attribute__((tc_blocking)) void foo();", "foo", True)
  , ("void foo();", "foo", False)
  ]
  where
    runTest (code, name, expected) = TestCase $
      expected @=? is_blocking_function (enrich code) (enrich name)

startTests = TestLabel "is_start_function" $ TestList $ map runTest [
    ("__attribute__((tc_run_thread)) void foo() { }", "foo", True)
  , ("void foo() { }", "foo", False)
  ]
  where
    runTest (code, name, expected) = TestCase $
      expected @=? is_start_function (enrich code) (enrich name)
