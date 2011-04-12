module Ocram.Analysis.Types (
	BlockingFunctions, 
	Callers, Callees, Entry(..), CallGraph,
	CriticalFunctions,
	FunctionMap,
	StartRoutines
) where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Language.C.Syntax.AST (CTranslUnit, CExtDecl, CTypeSpec, CFunDef)
import Ocram.Symbols (Symbol)

type AST = CTranslUnit

-- Blocking Functions
-- map of all blocking function declarations
type BlockingFunctions = Map.Map Symbol CExtDecl

-- Call Graph
-- caller: any function definition, callee: any function definition or any blocking function declaration

type Callers = Set.Set Symbol
type Callees = Set.Set Symbol
data Entry = Entry {cgCallers :: Callers, cgCallees :: Callees} deriving Show

type CallGraph = Map.Map Symbol Entry

-- Critical Functions
-- set of all critical functions
type CriticalFunctions = Set.Set Symbol

-- Function Map
-- map of all function definitions
type FunctionMap = Map.Map Symbol CFunDef

-- Start Routines
-- list of all start routine names

type StartRoutines = [Symbol]
