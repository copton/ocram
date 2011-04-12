module Ocram.Test.Tests.Analysis.CriticalFunctions (
	tests
) where

import Ocram.Test.Tests.Analysis.Utils (runTests)
import Ocram.Filter (checkSanity, checkRecursion)
import Ocram.Test.Lib (parse)
import Ocram.Analysis (getFunctions, determineBlockingFunctions, determineCallGraph, determineCriticalFunctions, findStartRoutines)
import Data.Set (empty, fromList)

reduce code = do
	raw_ast <- parse code
	sane_ast <- checkSanity raw_ast
	fm <- getFunctions sane_ast
	sr <- findStartRoutines fm
	bf <- determineBlockingFunctions sane_ast
	cg <- determineCallGraph sane_ast fm bf
	cf_ast <- checkRecursion sane_ast cg sr fm
	determineCriticalFunctions cf_ast cg fm bf	

tests = runTests "CriticalFunctions" reduce [
	 ("void foo() { }", empty)
	,("", empty)
	,("void foo();", empty)
	,("__attribute__((tc_blocking)) void foo();", fromList ["foo"])
	,("__attribute__((tc_blocking)) void foo(); void bar() { foo(); }", fromList ["foo", "bar"])
	,("__attribute__((tc_blocking)) void foo(); void bar(); void baz() { bar(); foo(); }", fromList ["baz", "foo"])
	,("__attribute__((tc_blocking)) void D(); void B() {D();} void C() {D();} void A() {B();C();}", fromList ["A", "B", "C", "D"])
	]
