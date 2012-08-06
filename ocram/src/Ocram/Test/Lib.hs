{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Ocram.Test.Lib where

-- imports {{{1
import Data.ByteString.Char8 (pack)
import Language.C.Data.Position (position)
import Language.C.Parser (parseC)
import Language.C.Pretty (pretty)
import Language.C.Syntax.AST (CTranslUnit)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH (stringE)
import Ocram.Analysis (CallGraph, ErrorCode, from_test_graph, to_test_graph)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework (testGroup, Test)
import Test.HUnit (Assertion)
import qualified Data.Set as Set
import qualified Data.List as List

parse :: String -> CTranslUnit -- {{{1
parse code = case parseC code' pos of
	Left e -> error $ show e
	Right ast -> ast
	where
		code' = pack code
		pos = position 0 "<<test>>" 0 0

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

instance TestData CallGraph TCallGraph where
	reduce cg = List.sort $ to_test_graph cg
	enrich cg = from_test_graph cg

instance TestData CTranslUnit String where
	reduce = show . pretty
	enrich = parse

instance TestData Char Char where
	reduce = id
	enrich = id

instance TestData ErrorCode Int where
	reduce = fromEnum
	enrich = toEnum

instance Ord a => TestData (Set.Set a) [a] where
	reduce set = Set.toList set
	enrich list = Set.fromList list

instance (TestData a b) => TestData [a] [b] where
	reduce = map reduce
	enrich = map enrich

-- types {{{1
type TCode = String
type TBlockingFunctions = [String]
type TCallGraph = [(String, String)]
type TStartFunctions = [String]
type TCriticalFunctions = [String]
type TErrorCodes = [ErrorCode]
type TCallChain = [String]
