module Analysis.Algorithms.CallGraph (
     determineCallGraph
) where

import Language.C.Syntax.AST
import Language.C.Data.Ident
import Data.Map as Map
import Util.Names (functionName)
import Visitor
import Analysis.Types.CallGraph
import Context

data State = State {
    stCaller :: Maybe CFunDef, 
    stCg :: CallGraph
}

emptyState = State Nothing Map.empty

instance Visitor State where
    handleCFunDef fd st = st { stCaller = Just fd }

--    handleCExpr (CCall (CVar (Ident name _ _))  _ _) = undefined
    handleCExpr _ = id 

determineCallGraph :: Context -> CallGraph
determineCallGraph ctx = stCg $ execTrav traverseCTranslUnit (ctxAst ctx) emptyState
