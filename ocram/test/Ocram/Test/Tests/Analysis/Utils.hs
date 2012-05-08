module Ocram.Test.Tests.Analysis.Utils where
-- imports {{{1
import Ocram.Types (Ast)
import Ocram.Test.Lib (parse, enumTestGroup, TErrorCodes, TCode)
import Ocram.Test.Tests.Analysis.TestCases (test_cases)
import Ocram.Test.Tests.Analysis.Types
import Test.HUnit ((@=?))
import Test.Framework (testGroup, Test)

runTests :: (Eq o, Show o) => TestType -> (Ast -> i -> o) -> (TCase -> (i, o)) -> Test -- {{{1
runTests test_type execute setup = enumTestGroup (show test_type) $ map (runTest . prepare) $ filter type_filter test_cases
  where
    prepare tc = (tcCode tc, setup tc)
    type_filter tc = test_type `notElem` (tcExclude tc)
    runTest (code, (input, output)) = output @=? execute (parse code) input

