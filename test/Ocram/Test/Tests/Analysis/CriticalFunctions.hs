module Ocram.Test.Tests.Analysis.CriticalFunctions (
	tests
) where

import Ocram.Test.Tests.Analysis.Utils (runTests)

import Ocram.Context (ctxCriticalFunctions)
import Ocram.Analysis.Types.FunctionMap (funId)
import Data.Set (elems)

reduce = (map funId).elems.ctxCriticalFunctions

tests = runTests "CriticalFunctions" reduce [
	 ("void foo() { }", [])
	,("", [])
	,("void foo();", [])
	,("__attribute__((tc_blocking)) void foo();", ["foo"])
	,("__attribute__((tc_blocking)) void foo(); void bar() { foo(); }", ["bar", "foo"])
	,("__attribute__((tc_blocking)) void foo(); void bar(); void baz { bar(); foo(); }", ["baz", "foo"])
	,("__attribute__((tc_blocking)) void D(); void B() {D();} void C() {D();} void A() {B();C();}", ["A","B","C","D"])
	]
