module Ocram.Test.Tests.Transformation.DataFlow (
	tests
) where

import Ocram.Test.Lib (createContext, parse')
import Ocram.Types (getAst, getStacklessAst)

import Test.HUnit (Test(TestLabel,TestCase,TestList), assertEqual)

import Language.C.Pretty (pretty)
import Language.C.Syntax.AST (CTranslationUnit(CTranslUnit))

tests = runTests [
	([
		"__attribute__((tc_blocking)) void foo(int i);",

		"__attribute__((tc_run_thread)) int bar(char param) {",
		"    int i;",
		"    foo(i);", 
		"}"
		],[
		"void foo(int i);",

		"int bar(char param) {",
		"    int i;",
		"    foo(i);", 
		"}",

		"typedef struct {",
		"    ec_continuation_t ec_cont;",
		"    int i;",
		"} ec_frame_foo_t;",

		"typedef struct {",
		"    ec_continuation_t ec_cont;",
		"    int ec_result;",
		"    union {",
		"        ec_frame_foo_t foo;",
    "    } ec_frames;",
		"    char param;",
    "    int i;",
		"} ec_frame_bar_t;"
	])
	]

runTests :: [([String], [String])] -> Test
runTests cases = TestLabel "DataFlow" $ TestList $ map runTest $ zip [1..] cases

runTest :: (Int, ([String], [String])) -> Test
runTest (number, (code, expected)) = TestCase $ assertEqual name expected' result
	where
		code' = unlines code
		expected' = show $ pretty $ strip $ getAst $ parse' $ unlines $ bootstrap : expected
		name = "test" ++ show number
		result = show $ pretty $ getAst ast
		ast = case getStacklessAst (createContext code' Nothing) of
			Left e -> error e
			Right x -> x
		bootstrap = "typedef struct { } ec_continuation_t;"
		strip(CTranslUnit (_:decls) ni) = CTranslUnit decls ni
