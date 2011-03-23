module Ocram.Test.Tests.Analysis.BlockingFunctions (
	tests
) where

import Ocram.Test.Tests.Analysis.Utils (runTests)

import Ocram.Context (ctxBlockingFunctions)
import Data.Map (keys)

reduce = keys.ctxBlockingFunctions

tests = runTests "BlockingFunctions" reduce [
	 ("void foo() { }", [])
	,("", [])
	,("void foo();", [])
	,("__attribute__((tc_blocking)) void foo();", ["foo"])
	]
