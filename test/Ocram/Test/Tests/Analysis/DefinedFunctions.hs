module Ocram.Test.Tests.Analysis.DefinedFunctions (
	tests
) where

import Ocram.Test.Lib
import Ocram.Types
import Ocram.Analysis.DefinedFunctions
import Data.Set (toList)

reduce :: String -> ER [String]
reduce code = do
	let ast = parse code
	df <- defined_functions ast
	return $ toList df

tests = runTests "DefinedFunctions" reduce [
	 ("void foo() { }", ["foo"])
	,("", [])
	,("int foo() {}", ["foo"])
	,("__attribute__((bar)) int foo() {}", ["foo"])
	,("void foo();", [])
	,("void foo(); void bar() { }", ["bar"])
	,("void foo() { } void bar() { }", ["bar", "foo"])
	]
