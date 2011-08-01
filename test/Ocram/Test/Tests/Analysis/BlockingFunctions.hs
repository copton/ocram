module Ocram.Test.Tests.Analysis.BlockingFunctions (
	tests
) where

import Ocram.Types
import Ocram.Test.Tests.Analysis.Types
import Ocram.Test.Tests.Analysis.Utils
import Ocram.Analysis.BlockingFunctions (blocking_functions)

type Input = ()
type Output = TBlockingFunctions

execute :: Ast -> Input -> ER Output
execute ast _ =
	return . reduce =<< blocking_functions ast

setup :: TCase -> (Input, Output)
setup tc = ((), tcBlockingFunctions tc)	

tests = runTests "BlockingFunctions" execute setup
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
