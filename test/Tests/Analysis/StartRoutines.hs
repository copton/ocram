module Tests.Analysis.StartRoutines (
    tests
) where 

import Test.HUnit
import Analysis.Algorithms.StartRoutines
import Analysis.Types.FunctionMap (funId)
import TestLib (parse)
import CreateContext (createContext)
import Context (ctxStartRoutines)

tests = TestLabel "StartRoutines" $ TestList [TestCase test1]

code1 = "__attribute__((tc_start_routine)) void foo() { }"

test1 = assertEqual "test1" expected result
    where
    ctx = createContext $ parse code1
    result = map funId $ ctxStartRoutines ctx
    expected = ["foo"]
