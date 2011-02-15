module Tests.Analysis.StartRoutines (
    tests
) where 

import Test.HUnit
import Context (ctxStartRoutines)
import Tests.Analysis.Utils (runTest)
import Analysis.Types.FunctionMap (funId)

tests = TestLabel "StartRoutines" $ TestList $ map runTest' $ zip [1..] test_list

test_list = [
    ("__attribute__((tc_run_thread)) void foo() { }", ["foo"])
   ,("void __attribute__((tc_run_thread)) foo() { }", ["foo"])
   ,("", [])
   ,("void foo() {}", [])
    ]


runTest' = runTest $ (map funId).ctxStartRoutines
