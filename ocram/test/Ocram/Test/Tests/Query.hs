module Ocram.Test.Tests.Query
-- exports {{{1
(
	tests
) where

-- import {{{1 
import Ocram.Query
import Ocram.Test.Lib (enumTestGroup, enrich, reduce)
import Test.Framework (testGroup)
import Test.HUnit ((@=?))
import Data.Maybe (isJust)

tests = -- {{{1
  testGroup "Query" [funDefTests, funDeclTests, blockingTests, startTests]


funDefTests = enumTestGroup "function_definition" $ map runTest [
    ("void foo() { }", "foo", True)
  , ("void bar() { }", "foo", False)
  , ("void foo();", "foo", False)
  ]
  where
    runTest (code, name, expected) = expected @=? (isJust $ function_definition (enrich code) (enrich name))


funDeclTests = enumTestGroup "function_declaration" $ map runTest [
    ("void foo();", "foo", True)
  , ("void bar() { }", "foo", False)
  , ("void foo() { }", "foo", False)
  ]
  where
    runTest (code, name, expected) = expected @=? (isJust $ function_declaration (enrich code) (enrich name))

blockingTests = enumTestGroup "is_blocking_function" $ map runTest [
    ("__attribute__((tc_blocking)) void foo();", "foo", True)
  , ("void foo();", "foo", False)
  ]
  where
    runTest (code, name, expected) = expected @=? is_blocking_function (enrich code) (enrich name)

startTests = enumTestGroup "is_start_function" $ map runTest [
    ("__attribute__((tc_run_thread)) void foo() { }", "foo", True)
  , ("void foo() { }", "foo", False)
  ]
  where
    runTest (code, name, expected) = expected @=? is_start_function (enrich code) (enrich name)
