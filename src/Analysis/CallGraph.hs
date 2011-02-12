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
data Entry = Entry {funDef :: CFunDef, callers :: Callers, callees :: Callees} deriving Show
type CallGraph = Map.Map String Entry

data State = State {
    stCaller :: Maybe CFunDef, 
    stCg :: CallGraph
}

emptyState = State Nothing Map.empty

instance Visitor State where
    handleCFunDef fd st = st { stCaller = Just fd }

--    handleCExpr (CCall (CVar (Ident name _ _))  _ _) = undefined
    handleCExpr _ = id 

determineCallGraph :: CTranslUnit -> CallGraph
determineCallGraph ctu = stCg $ execTrav traverseCTranslUnit ctu emptyState
