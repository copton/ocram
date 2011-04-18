module Ocram.Test.Tests.Analysis.StartRoutines (
	tests
) where 

import Ocram.Types (getStartRoutines)
import Ocram.Test.Lib (createContext)
import Ocram.Test.Tests.Analysis.Utils (runTests)
import Ocram.Symbols (symbol)
import qualified Data.Set as Set

reduce code = do
	let ctx = createContext code Nothing
	sr <- getStartRoutines ctx
	return $ (Set.map symbol) sr

tests = runTests "StartRoutines" reduce
	[("__attribute__((tc_run_thread)) void foo() { }", Set.singleton "foo")
	,("void __attribute__((tc_run_thread)) foo() { }", Set.singleton "foo")
	,("", Set.empty)
	,("void foo() {}", Set.empty)]
