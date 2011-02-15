module Tests.Analysis.Utils (
    createContext, runTest
) where

import Context (Context)
import qualified CreateContext
import TestLib (parse)
import Test.HUnit 

createContext :: String -> Context
createContext code = CreateContext.createContext $ parse code

--runTest :: (Context -> a) -> (Int, (String, a)) -> Test
runTest reduce (number, (code, expected)) = TestCase $ assertEqual name expected $ reduce result
    where
    name = "test" ++ show number
    result = createContext code

