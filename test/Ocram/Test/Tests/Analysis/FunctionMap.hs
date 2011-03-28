module Ocram.Test.Tests.Analysis.FunctionMap (
	tests
) where

import Ocram.Test.Lib (parse)
import Ocram.Analysis (getFunctions)
import Ocram.Test.Tests.Analysis.Utils (runTests)
import Ocram.Symbols (symbol)
import Data.Map (keys)

reduce code = do
	ast <- parse code
	fm <- getFunctions ast
	return $ (map symbol).keys $ fm

tests = runTests "FunctionMap" reduce [
	 ("void foo() { }", ["foo"])
	,("", [])
	,("int foo() {}", ["foo"])
	,("__attribute__((bar)) int foo() {}", ["foo"])
	,("void foo();", [])
	,("void foo(); void bar() { }", ["bar"])
	,("void foo() { } void bar() { }", ["bar", "foo"])
	]
