module Ocram.Test.Tests.Analysis.Utils (
    createContext, runTest, runTests
) where

import Ocram.Context (Context)
import qualified Ocram.CreateContext as CC
import Ocram.Test.Lib (parse)
import Test.HUnit (Test(TestLabel,TestCase,TestList), assertEqual)

createContext :: String -> Context
createContext code = CC.createContext $ parse code

runTest :: (Eq a, Show a) => (Context -> a) -> (Int, (String, a)) -> Test
runTest reduce (number, (code, expected)) = TestCase $ assertEqual name expected $ reduce result
    where
    name = "test" ++ show number
    result = createContext code

runTests :: (Eq a, Show a) => String -> (Context -> a) -> [(String, a)] -> Test
runTests label reduce tests = TestLabel label $ TestList $ map (runTest reduce) $ zip [1..] tests
