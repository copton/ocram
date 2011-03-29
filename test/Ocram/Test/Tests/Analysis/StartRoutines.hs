module Ocram.Test.Tests.Analysis.StartRoutines (
	tests
) where 

import Ocram.Analysis (getFunctions, findStartRoutines)
import Ocram.Filter (checkSanity)
import Ocram.Test.Lib (parse)
import Ocram.Test.Tests.Analysis.Utils (runTests)
import Ocram.Symbols (symbol)

reduce code = do
	raw_ast <- parse code
	sane_ast <- checkSanity raw_ast
	fm <- getFunctions sane_ast
	sr <- findStartRoutines fm
	return $ (map symbol) sr

tests = runTests "StartRoutines" reduce
	[("__attribute__((tc_run_thread)) void foo() { }", ["foo"])
	,("void __attribute__((tc_run_thread)) foo() { }", ["foo"])
	,("", [])
	,("void foo() {}", [])]
