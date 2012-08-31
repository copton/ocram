{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Ocram.Test.Lib where

-- imports {{{1
import Data.ByteString.Char8 (pack)
import Language.C.Data.Position (initPos)
import Language.C.Parser (parseC)
import Language.C.Pretty (pretty)
import Language.C.Syntax.AST (CTranslUnit)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH (stringE)
import Ocram.Analysis (CallGraph, ErrorCode, from_test_graph, to_test_graph)
import Ocram.Ruab (TLocation(..), ELocation(..), BlockingCall(..), TRow(..), ERow(..), VarMap(..), Variable(..), FunMap(..))
import Ocram.Debug (Location(..))
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework (testGroup, Test)
import Test.HUnit (Assertion)
import Text.Printf (printf)
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Map as M

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

instance TestData CallGraph TCallGraph where
	reduce cg = L.sort $ to_test_graph cg
	enrich cg = from_test_graph cg

instance TestData CTranslUnit String where
	reduce = show . pretty
	enrich = parse

instance TestData Location TstLocation where
  reduce bp = ((getTRow . tlocRow . bpTloc) bp, (getERow . elocRow . bpEloc) bp, bpThreadId bp)
  enrich (trow, erow, tid) = Location (TLocation (TRow trow) 1 1 "test") (ELocation (ERow erow) 1) tid

instance TestData BlockingCall TBlockingCall where
  reduce bc = ((getTRow . tlocRow . bcTloc) bc, (getERow . elocRow . bcEloc) bc, bcThreadId bc)
  enrich (trow, erow, tid) = BlockingCall (TLocation (TRow trow) 1 1 "test") (ELocation (ERow erow) 1) tid

instance TestData VarMap TVarMap where
  reduce = map tr . M.toList . getVarMap
    where tr (var, evar) = (varThread var, varFunction var, varSymbol var, evar)
  enrich = VarMap . M.fromList . map tr
    where tr (tid, fun, tvar, evar) = (Variable tid fun tvar, evar)

instance TestData FunMap TFunMap where
  reduce = map tr . M.toList . getFunMap
    where tr (fun, (start, end)) = (fun, getTRow start, getTRow end)
  enrich = FunMap . M.fromList . map tr
    where tr (fun, start, end) = (fun, (TRow start, TRow end))

instance TestData Char Char where
	reduce = id
	enrich = id

instance TestData ErrorCode Int where
	reduce = fromEnum
	enrich = toEnum

instance Ord a => TestData (S.Set a) [a] where
	reduce set = S.toList set
	enrich list = S.fromList list

instance (TestData a b) => TestData [a] [b] where
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
type TstLocation        = (Int, Int, Maybe Int)
type TstLocations       = [TstLocation]
type TBlockingCall      = (Int, Int, Int)
type TBlockingCalls     = [TBlockingCall]
type TVarMapEntry       = (Int, String, String, String)
type TVarMap            = [TVarMapEntry]
type TFunMapEntry       = (String, Int, Int)
type TFunMap            = [TFunMapEntry]
