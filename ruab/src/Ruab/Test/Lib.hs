{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Ruab.Test.Lib where

-- imports {{{1
import Control.Arrow ((***))
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH (stringE)
import Ocram.Ruab (PreprocMap(..), TRow(..), PRow(..))
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework (testGroup, Test)
import Test.HUnit (Assertion)
import Text.Printf (printf)

paste :: QuasiQuoter -- {{{1
paste = QuasiQuoter { 
	quoteExp = stringE, 
	quotePat = undefined,
	quoteType = undefined,
	quoteDec = undefined 
	}

enumTestGroup :: String -> [Assertion] -> Test -- {{{1
enumTestGroup name assertions = testGroup name $ zipWith (testCase . printf "%.2d") [(1 :: Int)..] assertions

class TestData d t where -- {{{1
	reduce :: d -> t
	enrich :: t -> d

instance TestData PreprocMap TPreprocMap where -- {{{2
  reduce (PreprocMap (TRow mtr) (PRow mpr) ma) = (mtr, mpr, map (getTRow *** getPRow) ma)
  enrich (mtr, mpr, ma) = PreprocMap (TRow mtr) (PRow mpr) (map (TRow *** PRow) ma)

type TPreprocMap = (Int, Int, [(Int, Int)])
