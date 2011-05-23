module Ocram.Test.Tests.Filter.CallGraph (
	tests
) where

import Ocram.Filter.CallGraph (getErrorCodes)
import Ocram.Test.Tests.Filter.Utils (runTests)
import Ocram.Test.Lib (createContext, paste)

reduce code = getErrorCodes (createContext code Nothing)

tests = runTests "CallGraph" reduce [
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
	|], [1]),
	([$paste|
		__attribute__((tc_blocking)) int block(int i);

		__attribute__((tc_run_thread)) void start() 
		{
			int i;
			i = block(i);
		}
	|], [])
	]
