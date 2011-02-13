module Analysis.Types.CallGraph (
    Callers, Callees, Entry, CallGraph
) where

import Language.C.Syntax.AST
import Data.Map as Map

type Callers = [CFunDef]
type Callees = [CFunDef]
data Entry = Entry {funDef :: CFunDef, callers :: Callers, callees :: Callees} deriving Show
type CallGraph = Map.Map String Entry
