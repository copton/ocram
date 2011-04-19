module Ocram.Test.Tests.Analysis.BlockingFunctions (
	tests
) where

import Ocram.Test.Lib (createContext)
import Ocram.Types (getBlockingFunctions)
import Ocram.Test.Tests.Analysis.Utils (runTests)
import Data.Set (empty, fromList)

reduce code = do
	let ctx = createContext code Nothing
	bf <- getBlockingFunctions ctx
	return bf
	
tests = runTests "BlockingFunctions" reduce [
	 ("void foo() { }", empty)
	,("", empty )
	,("void foo();", empty)
	,("__attribute__((tc_blocking)) void foo();", fromList ["foo"])
	]
