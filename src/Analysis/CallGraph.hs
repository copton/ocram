module Analysis.CallGraph (
     determineCallGraph
    , Callers, Callees, Entry, CallGraph
) where

import Language.C.Syntax.AST
import Language.C.Data.Ident
import Data.Map (Map, fromList)
import Util.Names (functionName)
import Util.Filter (funDefs)

type Callers = [CFunDef]
type Callees = [CFunDef]
data Entry = Entry {funDef :: CFunDef, callers :: Callers, callees :: Callees}
type CallGraph = Map String Entry

determineCallGraph :: CTranslUnit -> CallGraph
determineCallGraph ctu = fromList $ zip keys values
  where 
    fdefs = funDefs ctu
    keys = map functionName fdefs 
    values = map createEntry fdefs 
    createEntry fdef = Entry decl (callers fdef) []
    callers fdef = []
