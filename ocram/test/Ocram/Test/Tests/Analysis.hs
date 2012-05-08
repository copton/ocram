module Ocram.Test.Tests.Analysis (
	tests
) where

import Data.Maybe (isJust, fromJust)
import Ocram.Analysis (call_chain, call_order, get_callees)
import Ocram.Analysis.Fgl (find_loop)
import Ocram.Analysis.Types (CallGraph(..), Label(lblName))
import Ocram.Test.Lib (enrich, reduce, enumTestGroup)
import Test.HUnit ((@=?), (@?))
import Test.Framework (testGroup)

import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Map as Map
import qualified Ocram.Test.Tests.Analysis.TestSuites as T

tests = testGroup "Analysis" $ getCallChainTests : findLoopTests : callOrderTests : calleesTests : T.tests

getCallChainTests = enumTestGroup "call_chain" $ map runTest [
		([("a", "b")], ("a", "a"), ["a"])
  ,	([("a", "b")], ("a", "b"), ["a", "b"])
	,	([("a", "b"), ("a", "c")], ("a", "b"), ["a", "b"])
	,	([("a", "b"), ("a", "c")], ("a", "c"), ["a", "c"])
  ]
  where
    runTest (cg, (start, end), expected) = do
			let result = call_chain (enrich cg) (enrich start) (enrich end)
			isJust result @? "could not determine call chain."
			expected @=? (reduce $ fromJust result)

findLoopTests = enumTestGroup "find_loop" $ map runTest [
      ([("a", "b")], "a", Nothing)
    , ([("a", "b"), ("b", "a")], "a", Just ["a", "b", "a"])
    , ([("a", "b"), ("b", "c"), ("c", "b")], "a", Just ["a", "b", "c", "b"])
    , ([("a", "b"), ("b", "c"), ("b", "d"), ("d", "a")], "a", Just ["a", "b", "d", "a"])
  ]
  where
    runTest (cg, start, expected) =
      let
        (CallGraph gd gi) = enrich cg
        gstart = gi Map.! start
        gresult = find_loop gd gstart
        result = fmap (map (lblName . fromJust . G.lab gd)) gresult
      in
        expected @=? result
        
callOrderTests = enumTestGroup "call_order" $ map runTest [
    ([("a", "b")], "a", ["a", "b"])
  , ([("a", "b"), ("b", "c")], "a", ["a", "b", "c"])
  , ([("a", "b"), ("a", "c")], "a", ["a", "b", "c"])
  ]
  where
    runTest (cg, start, expected) = do
      let result = call_order (enrich cg) (enrich start)
      isJust result @? "could not determine call order"
      expected @=? (reduce $ fromJust result)

calleesTests = enumTestGroup "callees" $ map runTest [
    ([("a", "b")], "a", ["b"])
  , ([("a", "b")], "b", [])
  , ([("a", "b"), ("b", "c")], "b", ["c"])
  ]
  where
    runTest (cg, function, expected) = do
      let result = get_callees (enrich cg) (enrich function)
      isJust result @? "could not determine callees"
      expected @=? (reduce $ fromJust result)
