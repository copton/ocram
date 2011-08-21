module Ocram.Test.Tests.Analysis.CriticalFunctions (
	tests
) where

import Ocram.Types
import Ocram.Test.Tests.Analysis.Types
import Ocram.Test.Tests.Analysis.Utils
import Ocram.Analysis.CriticalFunctions (critical_functions)

type Input = (TCallGraph, TBlockingFunctions)
type Output = TCriticalFunctions

execute :: Ast -> Input -> Output
execute ast (cg, bf) =
	reduce $ critical_functions (enrich cg) (enrich bf) ast

setup :: TCase -> (Input, Output)
setup tc = ((tcCallGraph tc, tcBlockingFunctions tc), tcCriticalFunctions tc)

tests = runTests TTCriticalFunctions execute setup
