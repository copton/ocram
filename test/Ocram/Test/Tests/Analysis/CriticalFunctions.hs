module Ocram.Test.Tests.Analysis.CriticalFunctions (
	tests
) where

import Ocram.Test.Tests.Analysis.Utils (runTests)

import Ocram.Context (ctxCriticalFunctions)
import Ocram.Analysis.Types.FunctionMap (funId)
import Data.Set (elems)
import Data.List (sort)

data L = L [String]
instance Eq L where
	(L ss) == (L ss') = (sort ss) == (sort ss')
instance Show L where
	show (L ss) = show ss

reduce = L.(map funId).elems.ctxCriticalFunctions

tests = runTests "CriticalFunctions" reduce [
	 ("void foo() { }", L [])
	,("", L [])
	,("void foo();", L [])
	,("__attribute__((tc_blocking)) void foo();", L ["foo"])
	,("__attribute__((tc_blocking)) void foo(); void bar() { foo(); }", L ["foo", "bar"])
	,("__attribute__((tc_blocking)) void foo(); void bar(); void baz { bar(); foo(); }", L ["baz", "foo"])
	,("__attribute__((tc_blocking)) void D(); void B() {D();} void C() {D();} void A() {B();C();}", L ["A","B","C","D"])
	]
