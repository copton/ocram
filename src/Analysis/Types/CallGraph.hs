module Analysis.Types.CallGraph (
    Callers, Callees, Entry(..), CallGraph
) where

import Language.C.Syntax.AST
import Data.Map as Map

type Callers = [CFunDef]
type Callees = [CFunDef]
data Entry = Entry {cgFunDef :: CFunDef, cgCallers :: Callers, cgCallees :: Callees} deriving Show
type CallGraph = Map.Map String Entry
