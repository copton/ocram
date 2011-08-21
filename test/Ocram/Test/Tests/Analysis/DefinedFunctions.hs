module Ocram.Test.Tests.Analysis.DefinedFunctions (
	tests
) where

import Ocram.Types
import Ocram.Test.Tests.Analysis.Types
import Ocram.Test.Tests.Analysis.Utils
import Ocram.Analysis.DefinedFunctions

type Input = ()
type Output = TDefinedFunctions

execute :: Ast -> Input -> Output
execute ast _ = reduce $ defined_functions ast

setup :: TCase -> (Input, Output)
setup tc = ((), tcDefinedFunctions tc)

tests = runTests TTDefinedFunctions execute setup
