module Ocram.Test.Tests.Filter.CallGraph.Test1 (
	tests
) where

import Ocram.Filter.CallGraph (getErrorCodes)
import Ocram.Test.Tests.Filter.Utils (runTests)
import Ocram.Test.Lib (createContext, paste)

reduce code = getErrorCodes (createContext code Nothing)

tests = runTests "Test1" reduce [
	([$paste|
		void foo() {
			bar();
		} 

		void bar() {
			foo();
		}
	|], []),
	([$paste|
		__attribute__((tc_blocking)) void block(); 
		__attribute__((tc_run_thread)) void start() {
			rec();
		} 
		void rec() {
			block(); 
			start();
		}
	|], [1])
	]
