module Ocram.Test.Lib (
	parse, paste, runTests, runTestsWithOptions
) where

import Ocram.Types
import Ocram.Options (defaultOptions, Options)
import qualified Data.ByteString.Char8 as B
import Language.C.Data.Position (position)
import Language.C.Parser (parseC)

import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH (stringL, litE, litP)

import Test.HUnit (Test(TestLabel,TestCase,TestList), assertEqual)

parse :: String -> Ast
parse code = case parseC code' pos of
	Left e -> error $ show e
	Right ast -> ast
	where
		code' = B.pack code
		pos = position 0 "<<test>>" 0 0

paste = QuasiQuoter (litE . stringL) (litP . stringL) 

runTest :: (Eq o, Show o) => (i -> ER o) -> (Int, (i, Options, o)) -> Test
runTest reduce (number, (input, options, output)) = TestCase $ assertEqual name output result
	where
		name = "test" ++ show number
		result = case execER options (reduce input) of
			Left e -> error e
			Right x -> x

runTests :: (Eq o, Show o) => String -> (i -> ER o) -> [(i, o)] -> Test 
runTests label reduce tests = runTestsWithOptions label reduce $ map (\(i, o) -> (i, defaultOptions, o)) tests

runTestsWithOptions :: (Eq o, Show o) => String -> (i -> ER o) -> [(i, Options, o)] -> Test
runTestsWithOptions label reduce tests = TestLabel label $ TestList $ map (runTest reduce) $ zip [1..] tests
