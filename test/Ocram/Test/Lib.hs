module Ocram.Test.Lib where

-- imports {{{1
import Ocram.Types

import Ocram.Symbols (symbol)

import Language.C.Data.Position (position)
import Language.C.Parser (parseC)

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

type TCode = String
type TBlockingFunctions = [String]
type TDefinedFunctions = [String]
type TCallGraph = [(String, [String], [String])]
type TStartRoutines = [String]
type TCriticalFunctions = [String]
type TErrorCodes = [Int]
