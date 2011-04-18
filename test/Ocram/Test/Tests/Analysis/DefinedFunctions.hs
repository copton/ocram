module Ocram.Test.Tests.Analysis.DefinedFunctions (
	tests
) where

import Ocram.Test.Lib (createContext)
import Ocram.Types (getDefinedFunctions)
import Ocram.Test.Tests.Analysis.Utils (runTests)
import Ocram.Symbols (symbol)
import Data.Set (empty, fromList)

reduce code = do
	let ctx = createContext code Nothing
	df <- getDefinedFunctions ctx
	return df

tests = runTests "DefinedFunctions" reduce [
	 ("void foo() { }", fromList ["foo"])
	,("", empty)
	,("int foo() {}", fromList ["foo"])
	,("__attribute__((bar)) int foo() {}", fromList ["foo"])
	,("void foo();", empty)
	,("void foo(); void bar() { }", fromList ["bar"])
	,("void foo() { } void bar() { }", fromList ["bar", "foo"])
	]
