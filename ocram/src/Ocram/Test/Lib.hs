{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Ocram.Test.Lib where

-- imports {{{1
import Control.Applicative ((<$>), (<*>))
import Control.Arrow ((***))
import Data.ByteString.Char8 (pack)
import Language.C.Data.Position (initPos)
import Language.C.Parser (parseC)
import Language.C.Pretty (pretty)
import Language.C.Syntax.AST (CTranslUnit)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH (stringE)
import Ocram.Analysis (CallGraph, ErrorCode, from_test_graph, to_test_graph)
import Ocram.Ruab
import Ocram.Debug (Breakpoint(..))
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework (testGroup, Test)
import Test.HUnit (Assertion)
import Text.Printf (printf)
import qualified Data.List as L

parse :: String -> CTranslUnit -- {{{1
parse code = case parseC code' pos of
	Left e -> error $ show e
	Right ast -> ast
	where
		code' = pack code
		pos = initPos "<<test>>"

paste :: QuasiQuoter -- {{{1
paste = QuasiQuoter { 
	quoteExp = stringE, 
	quotePat = undefined,
	quoteType = undefined,
	quoteDec = undefined 
	}

lpaste :: QuasiQuoter -- {{{1
lpaste = QuasiQuoter { 
	quoteExp = stringE . reverse . drop 2 . reverse . unlines . map (drop 4) . tail . lines,
	quotePat = undefined,
	quoteType = undefined,
	quoteDec = undefined 
	}

enumTestGroup :: String -> [Assertion] -> Test -- {{{1
enumTestGroup name assertions = testGroup name $ zipWith (testCase . printf "%.2d") [(1 :: Int)..] assertions

class TestData d t where -- {{{1
	reduce :: d -> t
	enrich :: t -> d

instance TestData CallGraph TCallGraph where -- {{{2
	reduce cg = L.sort $ to_test_graph cg
	enrich cg = from_test_graph cg

instance TestData CTranslUnit String where -- {{{2
	reduce = show . pretty
	enrich = parse

instance TestData VarMap TVarMap where -- {{{2
  reduce = map tr
    where
      tr (scope, vars) = ((getPRow . scStart) scope, (getPRow . scEnd) scope, map tr' vars)
      tr' (var, fqn)   = (thread var, varTName var, fqn)
      thread v@(AutomaticVariable _ _) = Just (varThread v)
      thread   (StaticVariable _)      = Nothing

  enrich = map tr
    where
      tr (start, end, vars) = (Scope (PRow start) (PRow end), map tr' vars)
      tr' (Nothing, name, fqn) = (StaticVariable name, fqn)
      tr' (Just tid, name, fqn) = (AutomaticVariable tid name, fqn)

instance TestData MapTP TMapTP where -- {{{2
  reduce (MapTP (TRow mtr) (PRow mpr) ma) = (mtr, mpr, map (getTRow *** getPRow) ma)
  enrich (mtr, mpr, ma) = MapTP (TRow mtr) (PRow mpr) (map (TRow *** PRow) ma)

instance TestData Breakpoint TBreakpoint where -- {{{2
  reduce = (,,,) <$> getTRow . bpTRow <*> getERow . bpERow <*> bpThread <*> bpBlocking

  enrich (tr, er, tid, bl) = Breakpoint (TRow tr) (ERow er) tid bl

instance TestData Char Char where -- {{{2
	reduce = id
	enrich = id

instance TestData ErrorCode Int where -- {{{2
	reduce = fromEnum
	enrich = toEnum

instance (TestData a b) => TestData [a] [b] where -- {{{2
  reduce = map reduce
  enrich = map enrich

-- types {{{1
type TCode              = String
type TBlockingFunctions = [String]
type TCallGraph         = [(String, String)]
type TStartFunctions    = [String]
type TCriticalFunctions = [String]
type TErrorCodes        = [ErrorCode]
type TCallChain         = [String]
type TVarMap            = [(Int, Int, [(Maybe Int, String, String)])]
type TMapTP             = (Int, Int, [(Int, Int)])
type TBreakpoint        = (Int, Int, Maybe Int, Bool)
