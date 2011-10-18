module Ocram.Test.Tests.Analysis.TestSuites 
-- exports {{{1
(
	tests
) where
-- imports {{{1
import Ocram.Analysis (start_functions, blocking_functions, critical_functions, call_graph, check_sanity, check_constraints)
import Ocram.Test.Lib
import Ocram.Test.Tests.Analysis.Types
import Ocram.Test.Tests.Analysis.Utils (runTests)
import Ocram.Text (OcramError(errCode))
import Ocram.Types

-- tests {{{1
tests = [sf_tests, bf_tests, cf_tests, cg_tests, con_tests, san_tests]

sf_tests = runTests TTStartFunctions execute setup 
	where
		execute _ cg = reduce $ start_functions $ enrich cg
		setup tc = (tcCallGraph tc, tcStartFunctions tc)

bf_tests = runTests TTBlockingFunctions execute setup
	where
		execute _ cg = reduce $ blocking_functions $ enrich cg
		setup tc = (tcCallGraph tc, tcBlockingFunctions tc)	

cf_tests = runTests TTCriticalFunctions execute setup
	where
		execute _ cg = reduce $ critical_functions (enrich cg)
		setup tc = (tcCallGraph tc, tcCriticalFunctions tc)

cg_tests = runTests TTCallGraph execute setup
	where
		execute ast _ = reduce $ call_graph ast
		setup tc = ((), tcCallGraph tc)

con_tests = runTests TTConstraints execute setup
	where
		execute ast cg = 
			case check_constraints ast (enrich cg) of
				Left es -> map errCode es
				Right _ -> []
		setup tc = (tcCallGraph tc, tcConstraints tc)

san_tests = runTests TTSanity execute setup
	where
		execute ast _ =
			case check_sanity ast of
				Left es -> map errCode es
				Right _ -> [] 
		setup tc = ((), tcSanity tc)
