module Ocram.Test.Lib where

-- imports {{{1
import Data.ByteString.Char8 (pack)
import Language.C.Data.Position (position)
import Language.C.Parser (parseC)
import Language.C.Pretty (pretty)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH (stringE)
import Ocram.Analysis (CallGraph)
import Ocram.Analysis.Functions (from_test_graph, to_test_graph)
import Ocram.Symbols (symbol, Symbol)
import Ocram.Types (Ast)
import qualified Data.Map as Map
import qualified Data.Set as Set

-- parse :: String -> Ast {{{1
parse :: String -> Ast
parse code = case parseC code' pos of
	Left e -> error $ show e
	Right ast -> ast
	where
		code' = pack code
		pos = position 0 "<<test>>" 0 0

-- paste = QuasiQuoter {{{1
paste = QuasiQuoter { 
	quoteExp = stringE, 
	quotePat = undefined,
	quoteType = undefined,
	quoteDec = undefined 
	}


-- types {{{1
class TestData d t where
	reduce :: d -> t
	enrich :: t -> d

instance TestData (Set.Set Symbol) [String] where
	reduce = Set.toList
	enrich = Set.fromList

instance TestData CallGraph TCallGraph where
	reduce cg = to_test_graph cg
	enrich cg = from_test_graph cg

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
type TCallGraph = [(String, String)]
type TStartFunctions = [String]
type TCriticalFunctions = [String]
type TErrorCodes = [Int]
type TCallChain = [String]
