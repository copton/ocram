module Ocram.Test.Tests.Analysis.FunctionMap (
	tests
) where

import Ocram.Test.Tests.Analysis.Utils (runTests)

import Ocram.Context (ctxFunctionMap)
import Ocram.Symbols (symbol)
import Data.Map (keys)

reduce = (map symbol).keys.ctxFunctionMap

tests = runTests "FunctionMap" reduce [
	 ("void foo() { }", ["foo"])
	,("", [])
	,("int foo() {}", ["foo"])
	,("void foo();", [])
	,("void foo(); void bar() { }", ["bar"])
	,("void foo() { } void bar() { }", ["bar", "foo"])
	]
