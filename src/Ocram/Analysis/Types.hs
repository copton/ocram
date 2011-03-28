module Ocram.Analysis.Types (
	BlockingFunctions, 
	Callers, Callees, Entry(..), CallGraph,
	Signature(Signature), CriticalFunctions,
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
type BlockingFunctions = Map.Map String CExtDecl

-- Call Graph
-- caller: any function definition, callee: any function definition or any blocking function declaration

type Callers = Set.Set Symbol
type Callees = Set.Set Symbol
data Entry = Entry {cgCallers :: Callers, cgCallees :: Callees} deriving Show

type CallGraph = Map.Map Symbol Entry

-- Critical Functions
-- map of all critical function signatures
data Signature = Signature CTypeSpec [(CTypeSpec, Symbol)]

type CriticalFunctions = Map.Map Symbol Signature

-- Function Map
-- map of all function definitions
type FunctionMap = Map.Map String CFunDef

-- Start Routines
-- list of all start routine names

type StartRoutines = [Symbol]
