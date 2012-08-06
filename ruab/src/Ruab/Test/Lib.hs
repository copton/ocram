{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Ruab.Test.Lib where

-- imports {{{1
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH (stringE)
import Ocram.Ruab (PreprocMap(..))
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework (testGroup, Test)
import Test.HUnit (Assertion)

paste :: QuasiQuoter -- {{{1
paste = QuasiQuoter { 
	quoteExp = stringE, 
	quotePat = undefined,
	quoteType = undefined,
	quoteDec = undefined 
	}

enumTestGroup :: String -> [Assertion] -> Test -- {{{1
enumTestGroup name assertions = testGroup name $ zipWith (testCase . show) [(1 :: Int)..] assertions

class TestData d t where -- {{{1
	reduce :: d -> t
	enrich :: t -> d

instance TestData PreprocMap TPreprocMap where -- {{{2
  reduce (PreprocMap w x y) = (w, x, y)
  enrich (w, x, y) = PreprocMap w x y

type TPreprocMap = (Int, Int, [(Int, Int)])
