module Ocram.Test.Tests.Analysis.CallGraph (
	tests
) where

import Ocram.Types
import Ocram.Test.Tests.Analysis.Types
import Ocram.Test.Tests.Analysis.Utils
import Ocram.Analysis.CallGraph (call_graph)

type Input = (TDefinedFunctions, TBlockingFunctions)
type Output = TCallGraph

execute :: Ast -> Input -> ER Output
execute ast (df, bf) =
	return . reduce =<< call_graph (enrich df) (enrich bf) ast

setup :: TCase -> (Input, Output)
setup tc = ((tcDefinedFunctions tc, tcBlockingFunctions tc), tcCallGraph tc)

tests = runTests TTCallGraph execute setup
