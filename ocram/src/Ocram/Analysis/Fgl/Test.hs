module Ocram.Analysis.Fgl.Test (tests) where

import Data.Maybe (fromJust)
import Ocram.Analysis.Fgl (find_loop)
import Ocram.Analysis.Types (CallGraph(CallGraph), Label(lblName))
import Ocram.Test.Lib (enrich, enumTestGroup)
import Test.Framework (testGroup, Test)
import Test.HUnit ((@=?))

import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Map as Map

tests :: Test
tests = testGroup "Fgl" [test_find_loop]

test_find_loop :: Test
test_find_loop = enumTestGroup "find_loop" $ map runTest [
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
