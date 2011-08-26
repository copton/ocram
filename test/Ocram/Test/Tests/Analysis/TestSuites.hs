module Ocram.Test.Tests.Analysis.TestSuites 
-- exports {{{1
(
	tests
) where
-- imports {{{1

import Ocram.Types
import Ocram.Test.Lib
import Ocram.Test.Tests.Analysis.Types
import Ocram.Test.Tests.Analysis.Utils
import Ocram.Analysis

-- test suites {{{1
tests = [sr_tests, adg_tests, bf_tests, cg_tests, con_tests, cf_tests, df_tests, san_tests]

sr_tests = runTests TTStartRoutines execute setup 
	where
		execute ast df = reduce $ start_routines (enrich df) ast
		setup tc = (tcDefinedFunctions tc, tcStartRoutines tc)

adg_tests = runTests TTADG execute setup
	where
		execute ast (cg, sr, df) = 
			case check_call_graph (enrich cg) (enrich sr) (enrich df) ast of
				Left e -> extractErrorCodes e
				Right _ -> []
		setup tc = ((tcCallGraph tc, tcStartRoutines tc, tcDefinedFunctions tc), tcADG tc)

bf_tests = runTests TTBlockingFunctions execute setup
	where
		execute ast _ = reduce $ blocking_functions ast
		setup tc = ((), tcBlockingFunctions tc)	

cg_tests = runTests TTCallGraph execute setup
	where
		execute ast (df, bf) = reduce $ call_graph (enrich df) (enrich bf) ast
		setup tc = ((tcDefinedFunctions tc, tcBlockingFunctions tc), tcCallGraph tc)

con_tests = runTests TTConstraints execute setup
	where
		execute ast (cf, sr) = 
			case check_constraints (enrich cf) (enrich sr) ast of
				Left e -> extractErrorCodes e
				Right _ -> []
		setup tc = ((tcCriticalFunctions tc, tcStartRoutines tc), tcConstraints tc)

cf_tests = runTests TTCriticalFunctions execute setup
	where
		execute ast (cg, bf) = reduce $ critical_functions (enrich cg) (enrich bf) ast
		setup tc = ((tcCallGraph tc, tcBlockingFunctions tc), tcCriticalFunctions tc)

df_tests = runTests TTDefinedFunctions execute setup
	where
		execute ast _ = reduce $ defined_functions ast
		setup tc = ((), tcDefinedFunctions tc)

san_tests = runTests TTSanity execute setup
	where
		execute ast _ =
			case check_sanity ast of
				Left e -> extractErrorCodes e
				Right _ -> [] 
		setup tc = ((), tcSanity tc)
