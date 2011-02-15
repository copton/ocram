module Tests.Analysis.FunctionMap (
    tests
) where

import Test.HUnit
import Context (ctxFunctionMap)
import Tests.Analysis.Utils (createContext)
import Analysis.Types.FunctionMap (funId)
import Data.Map (keys)

tests = TestLabel "FunctionMap" $ TestList [TestCase test1]

test1 = assertEqual "test1" expected result
    where 
    code = "void foo() { }"
    expected = ["foo"]
    result' = ctxFunctionMap $ createContext code
    result = map funId $ keys result'
