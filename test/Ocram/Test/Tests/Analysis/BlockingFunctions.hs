module Ocram.Test.Tests.Analysis.BlockingFunctions (
	tests
) where

import Ocram.Types
import Ocram.Test.Tests.Analysis.Types
import Ocram.Test.Tests.Analysis.Utils
import Ocram.Analysis.BlockingFunctions (blocking_functions)

type Input = ()
type Output = TBlockingFunctions

reduce :: Ast -> Input -> ER Output
reduce ast _ =
	return . reduce_bf =<< blocking_functions ast

setup :: TCase -> (Input, Output)
setup tc = ((), tcBlockingFunctions tc)	

tests = runTests "BlockingFunctions" reduce setup
--	 ("void foo() { }", [])
--	,("", [])
--	,("void foo();", [])
--	,("__attribute__((tc_blocking)) void foo();", ["foo"])
--	,("__attribute__((tc_blocking)) int foo();", ["foo"])
--	,("__attribute__((tc_blocking)) int foo(char);", ["foo"])
--	,("__attribute__((tc_blocking)) int foo(double,...);", ["foo"])
--	,([$paste|
--		__attribute__((tc_blocking)) int block(int i);
--
--		__attribute__((tc_run_thread)) void start() 
--		{
--			int i;
--			i = block(i);
--		}
--	|], ["block"])
--	]
