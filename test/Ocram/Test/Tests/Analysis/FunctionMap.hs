module Ocram.Test.Tests.Analysis.FunctionMap (
	tests
) where

import Ocram.Test.Tests.Analysis.Utils (runTests)

import Ocram.Context (ctxFunctionMap)
import Ocram.Analysis.Types.FunctionMap (funId)
import Data.Map (keys)

reduce = (map funId).keys.ctxFunctionMap

tests = runTests "FunctionMap" reduce [
	 ("void foo() { }", ["foo"])
	,("", [])
	,("int foo() {}", ["foo"])
	,("void foo();", [])
	]
