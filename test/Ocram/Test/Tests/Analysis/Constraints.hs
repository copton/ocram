module Ocram.Test.Tests.Analysis.Constraints (
	tests
) where

import Ocram.Types
import Ocram.Test.Tests.Analysis.Types
import Ocram.Test.Tests.Analysis.Utils
import Ocram.Analysis.Constraints (check_constraints)
import Control.Monad.Reader (ask)

type Input = (TCriticalFunctions, TStartRoutines)
type Output = TErrorCodes

execute :: Ast -> Input -> Output
execute ast (cf, sr) = 
	case check_constraints (enrich cf) (enrich sr) ast of
		Left e -> extractErrorCodes e
		Right _ -> []

setup :: TCase -> (Input, Output)
setup tc = ((tcCriticalFunctions tc, tcStartRoutines tc), tcConstraints tc)

tests = runTests TTConstraints execute setup
