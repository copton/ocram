module Analysis.CallGraph (
     determineCallGraph
    , Callers, Callees, Entry, CallGraph
) where

import Language.C.Syntax.AST
import Language.C.Data.Ident
import Data.Map as Map
import Util.Names (functionName)
import Util.Filter (funDefs)
import Visitor

type Callers = [CFunDef]
type Callees = [CFunDef]
data Entry = Entry {funDef :: CFunDef, callers :: Callers, callees :: Callees}
type CallGraph = Map.Map String Entry

data State = State {
    stCg :: CallGraph
}

instance Visitor State where
    handleCExpr (CCall _ _ _) = undefined
    handleCExpr _ = id 

determineCallGraph :: CTranslUnit -> CallGraph
determineCallGraph ctu = stCg $ execTrav traverseCTranslUnit ctu $ State Map.empty
