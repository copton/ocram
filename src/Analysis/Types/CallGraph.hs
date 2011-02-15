module Analysis.Types.CallGraph (
    Callers, Callees, Entry(..), CallGraph
) where

import Language.C.Syntax.AST
import Data.Map as Map
import Data.Set as Set
import Analysis.Types.FunctionMap (FunctionId)

type Callers = Set FunctionId
type Callees = Set FunctionId
data Entry = Entry {cgCallers :: Callers, cgCallees :: Callees} deriving Show
type CallGraph = Map.Map FunctionId Entry
