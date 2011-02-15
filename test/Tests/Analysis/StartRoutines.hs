module Tests.Analysis.StartRoutines (
    tests
) where 

import Test.HUnit
import Context (ctxStartRoutines)
import Tests.Analysis.Utils (createContext)
import Analysis.Types.FunctionMap (funId)

tests = TestLabel "StartRoutines" $ TestList [TestCase test1]

test1 = assertEqual "test1" expected result
    where
    code = "__attribute__((tc_run_thread)) void foo() { }"
    expected = ["foo"]
    result' = ctxStartRoutines $ createContext code
    result = map funId result'
