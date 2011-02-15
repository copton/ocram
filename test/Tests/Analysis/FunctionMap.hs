module Tests.Analysis.FunctionMap (
    tests
) where

import Tests.Analysis.Utils (runTests)

import Context (ctxFunctionMap)
import Analysis.Types.FunctionMap (funId)
import Data.Map (keys)

reduce = (map funId).keys.ctxFunctionMap

tests = runTests "FunctionMap" reduce [
     ("void foo() { }", ["foo"])
    ,("", [])
    ,("int foo() {}", ["foo"])
    ,("void foo();", [])
    ]
