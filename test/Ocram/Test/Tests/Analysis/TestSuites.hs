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
tests = [cg_tests, con_tests, san_tests]

cg_tests = runTests TTCallGraph execute setup
	where
		execute ast _ = reduce $ call_graph ast
		setup tc = ((), tcCallGraph tc)

con_tests = runTests TTConstraints execute setup
	where
		execute ast _ = 
			case check_constraints ast $ call_graph ast of
				Left es -> enrich $ map errCode es
				Right _ -> []
		setup tc = ((), tcConstraints tc)

san_tests = runTests TTSanity execute setup
	where
		execute ast _ =
			case check_sanity ast of
				Left es -> enrich $ map errCode es
				Right _ -> [] 
		setup tc = ((), tcSanity tc)
