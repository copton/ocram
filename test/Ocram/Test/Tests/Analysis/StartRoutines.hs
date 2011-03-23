module Ocram.Test.Tests.Analysis.StartRoutines (
	tests
) where 

import Ocram.Test.Tests.Analysis.Utils (runTests)

import Ocram.Context (ctxStartRoutines)
import Ocram.Symbols (symbol)

reduce = (map symbol).ctxStartRoutines

tests = runTests "StartRoutines" reduce
	[("__attribute__((tc_run_thread)) void foo() { }", ["foo"])
	,("void __attribute__((tc_run_thread)) foo() { }", ["foo"])
	,("", [])
	,("void foo() {}", [])]
