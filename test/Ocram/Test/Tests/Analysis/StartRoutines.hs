module Ocram.Test.Tests.Analysis.StartRoutines (
	tests
) where 

import Ocram.Types
import Ocram.Test.Tests.Analysis.Types
import Ocram.Test.Tests.Analysis.Utils
import Ocram.Analysis.StartRoutines (start_routines)

type Input = TDefinedFunctions
type Output = TStartRoutines

execute :: Ast -> Input -> Output
execute ast df = reduce $ start_routines (enrich df) ast

setup :: TCase -> (Input, Output)
setup tc = (tcDefinedFunctions tc, tcStartRoutines tc)

tests = runTests TTStartRoutines execute setup 
