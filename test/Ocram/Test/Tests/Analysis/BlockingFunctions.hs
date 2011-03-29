module Ocram.Test.Tests.Analysis.BlockingFunctions (
	tests
) where

import Ocram.Test.Lib (parse)
import Ocram.Filter (checkSanity)
import Ocram.Test.Tests.Analysis.Utils (runTests)
import Ocram.Analysis (determineBlockingFunctions)

import Data.Map (keys)

reduce code = do
	raw_ast <- parse code
	sane_ast <- checkSanity raw_ast
	bf <- determineBlockingFunctions sane_ast
	return $ keys bf
	
tests = runTests "BlockingFunctions" reduce [
	 ("void foo() { }", [])
	,("", [])
	,("void foo();", [])
	,("__attribute__((tc_blocking)) void foo();", ["foo"])
	]
