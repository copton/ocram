module Ocram.Test.Tests.Analysis (
	tests
) where

import Data.Maybe (isJust, fromJust)
import Ocram.Analysis (call_chain, call_order, callees)
import Ocram.Analysis.Fgl (find_loop)
import Ocram.Analysis.Types (CallGraph(..), Label(lblName))
import Ocram.Test.Lib
import Test.HUnit

import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Map as Map
import qualified Ocram.Test.Tests.Analysis.TestSuites as T

tests = TestLabel "Analysis" $ TestList $
  getCallChainTests : findLoopTests : callOrderTests : calleesTests : T.tests

getCallChainTests :: Test
getCallChainTests = TestLabel "call_chain" $ TestList $ map runTest [
		([("a", "b")], ("a", "a"), ["a"])
  ,	([("a", "b")], ("a", "b"), ["a", "b"])
	,	([("a", "b"), ("a", "c")], ("a", "b"), ["a", "b"])
	,	([("a", "b"), ("a", "c")], ("a", "c"), ["a", "c"])
  ]
  where
    runTest (cg, (start, end), expected) = TestCase $ do
			let result = call_chain (enrich cg) (enrich start) (enrich end)
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
        

callOrderTests :: Test
callOrderTests = TestLabel "call_order" $ TestList $ map runTest [
    ([("a", "b")], "a", ["a", "b"])
  , ([("a", "b"), ("b", "c")], "a", ["a", "b", "c"])
  , ([("a", "b"), ("a", "c")], "a", ["a", "b", "c"])
  ]
  where
    runTest (cg, start, expected) = TestCase $ do
      let result = call_order (enrich cg) (enrich start)
      isJust result @? "could not determine call order"
      expected @=? (reduce $ fromJust result)

calleesTests :: Test
calleesTests = TestLabel "callees" $ TestList $ map runTest [
    ([("a", "b")], "a", ["b"])
  , ([("a", "b")], "b", [])
  , ([("a", "b"), ("b", "c")], "b", ["c"])
  ]
  where
    runTest (cg, function, expected) = TestCase $ do
      let result = callees (enrich cg) (enrich function)
      isJust result @? "could not determine callees"
      expected @=? (reduce $ fromJust result)
