module Ocram.Test.Tests.Analysis.BlockingFunctions (
	tests
) where

import Ocram.Types
import Ocram.Test.Tests.Analysis.Types
import Ocram.Test.Tests.Analysis.Utils
import Ocram.Analysis.BlockingFunctions (blocking_functions)

type Input = ()
type Output = TBlockingFunctions

execute :: Ast -> Input -> Output
execute ast _ =
	reduce $ blocking_functions ast

setup :: TCase -> (Input, Output)
setup tc = ((), tcBlockingFunctions tc)	

tests = runTests TTBlockingFunctions execute setup
