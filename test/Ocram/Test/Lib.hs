module Ocram.Test.Lib where

-- imports {{{1
import Ocram.Types

import Ocram.Symbols (symbol)
import Ocram.Analysis.CallGraph (addCall)

import Language.C.Data.Position (position)
import Language.C.Parser (parseC)
import Language.C.Pretty (pretty)

import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH (stringE)

import Data.ByteString.Char8 (pack)
import qualified Data.Set as Set
import qualified Data.Map as Map

-- parse {{{1
parse :: String -> Ast
parse code = case parseC code' pos of
	Left e -> error $ show e
	Right ast -> ast
	where
		code' = pack code
		pos = position 0 "<<test>>" 0 0

-- paste {{{1
paste = QuasiQuoter { 
	quoteExp = stringE, 
	quotePat = undefined,
	quoteType = undefined,
	quoteDec = undefined 
	}


-- TestData {{{1
class TestData d t where
	reduce :: d -> t
	enrich :: t -> d

instance TestData (Set.Set Symbol) [String] where
	reduce = Set.toList
	enrich = Set.fromList

instance TestData CallGraph TCallGraph where
	reduce cg = map decompose (Map.toList cg)
		where
			decompose (function, (Entry callers callees)) =
				(function, Set.toList callers, Set.toList callees)

	enrich cg = foldl construct Map.empty cg
		where
			construct m (function, callers, callees) =
				Map.insert (symbol function) (Entry (convert callers) (convert callees)) m
			convert fs = Set.fromList (map symbol fs)

instance TestData CallGraph TCallGraphShort where
	reduce cg = concatMap decompose $ Map.toList cg
		where
			decompose (function, (Entry _ callees)) =
				[(function, callee) | callee <- Set.toList callees]
	enrich cg = foldl addCall Map.empty cg

instance TestData Ast String where
	reduce = show . pretty
	enrich = parse

instance TestData Symbol String where
	reduce = show
	enrich = symbol

instance TestData [Symbol] [String] where
	reduce = map reduce
	enrich = map enrich

data Call = String :-> String

type TCode = String
type TBlockingFunctions = [String]
type TDefinedFunctions = [String]
type TCallGraph = [(String, [String], [String])]
type TCallGraphShort = [(String, String)]
type TStartRoutines = [String]
type TCriticalFunctions = [String]
type TErrorCodes = [Int]
type TCallChain = [String]
