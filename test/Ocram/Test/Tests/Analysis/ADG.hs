module Ocram.Test.Tests.Analysis.ADG (
	tests
) where

import Ocram.Types
import Ocram.Test.Tests.Analysis.Types
import Ocram.Test.Tests.Analysis.Utils
import Ocram.Analysis.ADG (check_call_graph)
import Control.Monad.Reader (ask)

type Input = (TCallGraph, TStartRoutines, TDefinedFunctions)
type Output = TErrorCodes

execute :: Ast -> Input -> Output
execute ast (cg, sr, df) = 
	case check_call_graph (enrich cg) (enrich sr) (enrich df) ast of
		Left e -> extractErrorCodes e
		Right _ -> []

setup :: TCase -> (Input, Output)
setup tc = ((tcCallGraph tc, tcStartRoutines tc, tcDefinedFunctions tc), tcADG tc)

tests = runTests TTADG execute setup
