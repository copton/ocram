module Ocram.Query.Test
-- exports {{{1
(
	tests
) where

-- import {{{1 
import Ocram.Query
import Ocram.Test.Lib (enumTestGroup, enrich)
import Test.Framework (testGroup, Test)
import Test.HUnit ((@=?))
import Data.Maybe (isJust)
import Language.C.Syntax.AST (CTranslUnit)

tests :: Test
tests = -- {{{1
  testGroup "Query" [funDefTests, funDeclTests, blockingTests, startTests]

funDefTests :: Test
funDefTests = enumTestGroup "function_definition" $ map runTest [
    ("void foo() { }", "foo", True)
  , ("void bar() { }", "foo", False)
  , ("void foo();", "foo", False)
  ]
  where
    runTest (code, name, expected) =
      let ast = enrich code :: CTranslUnit in
      expected @=? isJust (function_definition ast (enrich name))

funDeclTests :: Test
funDeclTests = enumTestGroup "function_declaration" $ map runTest [
    ("void foo();", "foo", True)
  , ("void bar() { }", "foo", False)
  , ("void foo() { }", "foo", False)
  ]
  where
    runTest (code, name, expected) =
      let ast = enrich code :: CTranslUnit in
      expected @=? (isJust $ function_declaration ast (enrich name))

blockingTests :: Test
blockingTests = enumTestGroup "is_blocking_function" $ map runTest [
    ("__attribute__((tc_blocking)) void foo();", "foo", True)
  , ("void foo();", "foo", False)
  ]
  where
    runTest (code, name, expected) =
      let ast = enrich code :: CTranslUnit in
      expected @=? is_blocking_function ast (enrich name)

startTests :: Test
startTests = enumTestGroup "is_start_function" $ map runTest [
    ("__attribute__((tc_run_thread)) void foo() { }", "foo", True)
  , ("void foo() { }", "foo", False)
  ]
  where
    runTest (code, name, expected) =
      let ast = enrich code :: CTranslUnit in
      expected @=? is_start_function ast (enrich name)
