module Ocram.Test.Tests.Analysis.CriticalFunctions (
	tests
) where

import Ocram.Test.Tests.Analysis.Utils (runTests)
import Ocram.Types (getCriticalFunctions)
import Ocram.Test.Lib (createContext)
import Data.Set (empty, fromList)

reduce code = do
	let ctx = createContext code Nothing
	cf <- getCriticalFunctions ctx
	return cf

tests = runTests "CriticalFunctions" reduce [
	 ("void foo() { }", empty)
	,("", empty)
	,("void foo();", empty)
	,("__attribute__((tc_blocking)) void foo();", fromList ["foo"])
	,("__attribute__((tc_blocking)) void foo(); void bar() { foo(); }", fromList ["foo", "bar"])
	,("__attribute__((tc_blocking)) void foo(); void bar(); void baz() { bar(); foo(); }", fromList ["baz", "foo"])
	,("__attribute__((tc_blocking)) void D(); void B() {D();} void C() {D();} void A() {B();C();}", fromList ["A", "B", "C", "D"])
	]
