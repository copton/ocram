module Ocram.Types where

-- imports {{{1
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Writer

import Language.C.Syntax.AST (CTranslUnit, CDecl, CBlockItem, CTypeSpec, CExtDecl, CFunDef)
import qualified Data.Map as Map
import qualified Data.Set as Set

-- General {{{1
type Symbol = String
type FunMap a = Map.Map Symbol a
type Ast = CTranslUnit
data Options = Options { 
	  optInput :: String
	, optOutput :: String
	, optCppOptions :: String
	, optScheme :: String
	, optHelp :: Bool
} deriving Show

-- Monads {{{1
type EIO = ErrorT String IO

newtype ER a = ER {
		runER :: ErrorT String (Reader Options) a
	} deriving (
		Monad,
		MonadError String,
		MonadReader Options
	)

execER :: Options -> ER a -> Either String a
execER opt f = runReader (runErrorT (runER f)) opt

type WRData = (Options, Analysis)

newtype WR a = WR {
		runWR :: WriterT DebugSymbols (Reader WRData) a
 	} deriving (
		Monad, 
		MonadWriter DebugSymbols, 
		MonadReader WRData
	)

execWR :: WRData -> WR a -> (a, DebugSymbols)
execWR wrd f = runReader (runWriterT (runWR f)) wrd 
-- Analysis {{{1

-- map of all blocking function declarations
type BlockingFunctions = Set.Set Symbol

-- caller: any function definition
type Callers = Set.Set Symbol
-- callee: any function definition or any blocking function declaration
type Callees = Set.Set Symbol
data Entry = Entry {cgCallers :: Callers, cgCallees :: Callees} deriving Show

type CallGraph = Map.Map Symbol Entry

-- set of all critical functions
type CriticalFunctions = Set.Set Symbol

-- map of all function definitions
type DefinedFunctions = Set.Set Symbol 

-- list of all start routine names
type StartRoutines = Set.Set Symbol

data Analysis = Analysis {
	getBlockingFunctions :: BlockingFunctions,
	getDefinedFunctions :: DefinedFunctions,
	getStartRoutines :: StartRoutines,
	getCallGraph :: CallGraph,
	getCriticalFunctions :: CriticalFunctions
}

-- Transformation {{{1


data DebugSymbol = DebugSymbol -- TODO
type DebugSymbols = [DebugSymbol]



