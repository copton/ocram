module Ocram.Test.Tests.Analysis.DefinedFunctions (
	tests
) where

import Ocram.Test.Lib (parse)
import Ocram.Analysis (getFunctions)
import Ocram.Filter (checkSanity)
import Ocram.Test.Tests.Analysis.Utils (runTests)
import Ocram.Symbols (symbol)
import Data.Map (keys)

reduce code = do
	raw_ast <- parse code
	sane_ast <- checkSanity raw_ast
	fm <- getFunctions sane_ast
	return $ (map symbol).keys $ fm

tests = runTests "DefinedFunctions" reduce [
	 ("void foo() { }", ["foo"])
	,("", [])
	,("int foo() {}", ["foo"])
	,("__attribute__((bar)) int foo() {}", ["foo"])
	,("void foo();", [])
	,("void foo(); void bar() { }", ["bar"])
	,("void foo() { } void bar() { }", ["bar", "foo"])
	]
