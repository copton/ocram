module Tests.Analysis.StartRoutines (
    tests
) where 

import Tests.Analysis.Utils (runTests)

import Context (ctxStartRoutines)
import Analysis.Types.FunctionMap (funId)

reduce = (map funId).ctxStartRoutines

tests = runTests "StartRoutines" reduce [
    ("__attribute__((tc_run_thread)) void foo() { }", ["foo"])
   ,("void __attribute__((tc_run_thread)) foo() { }", ["foo"])
   ,("", [])
   ,("void foo() {}", [])
    ]
