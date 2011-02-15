module Tests.Analysis.StartRoutines (
    tests
) where 

import Test.HUnit

tests = TestLabel "StartRoutines" $ TestList [TestCase test1, TestCase test2]

test1 = assertEqual "test1" 1 1
test2 = assertEqual "test2" 1 0
