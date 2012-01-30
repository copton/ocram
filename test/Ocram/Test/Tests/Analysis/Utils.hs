module Ocram.Test.Tests.Analysis.Utils where
-- imports {{{1
import Ocram.Types (Ast)
import Ocram.Test.Lib (parse)
import Ocram.Test.Lib (TErrorCodes, TCode)
import Ocram.Test.Tests.Analysis.TestCases (test_cases)
import Ocram.Test.Tests.Analysis.Types
import Test.HUnit (Test(TestLabel,TestCase,TestList), assertEqual)

-- runTests :: (Eq o, Show o) => TestType -> (Ast -> i -> o) -> (TCase -> (i, o)) -> Test {{{1
runTests :: (Eq o, Show o) => TestType -> (Ast -> i -> o) -> (TCase -> (i, o)) -> Test
runTests test_type execute setup = TestLabel (show test_type) $ TestList $ map (runTest execute . prepare) $ filter type_filter $ zip [0..] test_cases
	where
		prepare (idx, tc) = (idx, (tcCode tc, setup tc))
		type_filter (_, tc) = test_type `notElem` (tcExclude tc)

runTest :: (Eq o, Show o) => (Ast -> i -> o) -> (Int, (TCode, (i, o))) -> Test
runTest execute (number, (code, (input, output))) = TestCase $ assertEqual name output result
	where
		name = "test" ++ show number
		ast = parse code
		result = execute ast input

