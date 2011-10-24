module Ocram.Test.Tests.Analysis (
	tests
) where

import Data.Maybe (isJust, fromJust)
import Ocram.Analysis (get_call_chain)
import Ocram.Analysis.Fgl (find_loop)
import Ocram.Analysis.Types (CallGraph(..), Label(lblName))
import Ocram.Test.Lib
import Test.HUnit

import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Map as Map
import qualified Ocram.Test.Tests.Analysis.TestSuites as T

tests = TestLabel "Analysis" $ TestList $
  getCallChainTests : findLoopTests : T.tests

getCallChainTests :: Test
getCallChainTests = TestLabel "get_call_chain" $ TestList $ map runTest [
		([("a", "b")], ("a", "b"), ["a", "b"])
	,	([("a", "b"), ("a", "c")], ("a", "b"), ["a", "b"])
	,	([("a", "b"), ("a", "c")], ("a", "c"), ["a", "c"])
  ]
  where
    runTest (cg, (start, end), expected) = TestCase $ do
			let result = get_call_chain (enrich cg) (enrich start) (enrich end)
			isJust result @? "could not determine call chain."
			expected @=? (reduce $ fromJust result)


findLoopTests :: Test
findLoopTests = TestLabel "find_loop" $ TestList $ map runTest [
      ([("a", "b")], "a", Nothing)
    , ([("a", "b"), ("b", "a")], "a", Just ["a", "b", "a"])
    , ([("a", "b"), ("b", "c"), ("c", "b")], "a", Just ["a", "b", "c", "b"])
    , ([("a", "b"), ("b", "c"), ("b", "d"), ("d", "a")], "a", Just ["a", "b", "d", "a"])
  ]
  where
    runTest (cg, start, expected) = TestCase $
      let
        (CallGraph gd gi) = enrich cg
        gstart = gi Map.! start
        gresult = find_loop gd gstart
        result = fmap (map (lblName . fromJust . G.lab gd)) gresult
      in
        expected @=? result
        
