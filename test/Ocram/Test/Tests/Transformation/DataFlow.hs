module Ocram.Test.Tests.Transformation.DataFlow (
	tests
) where

import Ocram.Test.Lib (parse')
import Ocram.Types (getAst)
import Ocram.Analysis (determineBlockingFunctions, getFunctions, findStartRoutines, determineCallGraph, determineCriticalFunctions)
import Ocram.Filter (checkSanity, checkConstraints, checkRecursion)
import Ocram.Transformation (transformDataFlow)

import Test.HUnit (Test(TestLabel,TestCase,TestList), assertEqual)

import Language.C.Pretty (pretty)
import Language.C.Syntax.AST (CTranslationUnit(CTranslUnit))

tests = runTests [
	([
		"__attribute__((tc_blocking)) void foo();",

		"int bar(char param) {",
		"    foo();", 
		"}"
		],[
		"typedef struct {",
		"    ec_continuation_t ec_cont;",
		"} frame_foo_t;",

		"typedef struct {",
		"    ec_continuation_t ec_cont;",
		"    int ec_result;",
		"    char param;",
		"} frame_bar_t;"
	])
	]

runTests :: [([String], [String])] -> Test
runTests cases = TestLabel "DataFlow" $ TestList $ map runTest $ zip [1..] cases

runTest :: (Int, ([String], [String])) -> Test
runTest (number, (code, expected)) = TestCase $ assertEqual name expected' result
	where
		code' = unlines code
		expected' = show $ pretty $ strip $ getAst $ parse' $ unlines $ bootstrap : code ++ expected
		name = "test" ++ show number
		result = show $ pretty $ getAst ast
		ast = case getStacklessAst code' of
			Left e -> error e
			Right x -> x
		bootstrap = "typedef struct { } ec_continuation_t;"
		strip(CTranslUnit (_:decls) ni) = CTranslUnit decls ni

getStacklessAst code = do
	let raw_ast = parse' code
	sane_ast <- checkSanity raw_ast
	blocking_functions <- determineBlockingFunctions sane_ast
	function_map <- getFunctions sane_ast
	start_routines <- findStartRoutines function_map
	call_graph <- determineCallGraph sane_ast function_map blocking_functions
	cyclefree_ast <- checkRecursion sane_ast call_graph start_routines function_map
	cricitcal_functions <- determineCriticalFunctions cyclefree_ast call_graph function_map blocking_functions
	critical_functions <- determineCriticalFunctions cyclefree_ast call_graph function_map blocking_functions
	valid_ast <- checkConstraints critical_functions cyclefree_ast
	(function_infos, stackless_ast) <- transformDataFlow valid_ast call_graph critical_functions blocking_functions function_map
	return stackless_ast
			
