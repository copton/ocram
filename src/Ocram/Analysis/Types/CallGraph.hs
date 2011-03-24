module Ocram.Analysis.Types.CallGraph (
    Callers, Callees, Entry(..), CallGraph
) where

import Language.C.Syntax.AST
import Data.Map as Map
import Data.Set as Set
import Ocram.Symbols (Symbol)

type Callers = Set Symbol
type Callees = Set Symbol
data Entry = Entry {cgCallers :: Callers, cgCallees :: Callees} deriving Show

-- call graph of all critical functions
type CallGraph = Map.Map Symbol Entry
