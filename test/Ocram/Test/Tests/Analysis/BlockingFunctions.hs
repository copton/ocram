module Ocram.Test.Tests.Analysis.BlockingFunctions (
	tests
) where

import Ocram.Test.Lib (parse)
import Ocram.Test.Tests.Analysis.Utils (runTests)
import Ocram.Analysis (determineBlockingFunctions)

import Data.Map (keys)

reduce code = do
	ast <- parse code
	bf <- determineBlockingFunctions ast
	return $ keys bf
	
tests = runTests "BlockingFunctions" reduce [
	 ("void foo() { }", [])
	,("", [])
	,("void foo();", [])
	,("__attribute__((tc_blocking)) void foo();", ["foo"])
	]
