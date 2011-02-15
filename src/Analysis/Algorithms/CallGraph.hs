module Analysis.Algorithms.CallGraph (
     determineCallGraph
) where

import Language.C.Syntax.AST
import Language.C.Data.Ident
import Data.Map (update, lookup, member, mapMaybe)
import Prelude hiding (lookup)
import Data.Maybe (fromJust)
import Util.Names (functionName)
import Visitor
import Analysis.Types.CallGraph
import Analysis.Types.FunctionMap
import Context

data State = State {
    stCaller :: Maybe CFunDef, 
    stCg :: CallGraph,
    stFunctions :: FunctionMap
}

emptyState ctx = State Nothing cg fm
    where
        fm = ctxFunctionMap ctx
        cg = mapMaybe convert fm
        convert fd = Just $ Entry fd [] []

instance Visitor State where
    handleCFunDef fd st = st { stCaller = Just fd }

    handleCExpr (CCall (CVar (Ident name _ _) _)  _ _) st 
        | member name (stFunctions st) = st {stCg = update update' caller (stCg st)}
        | otherwise = st
          where 
            update' entry = Just $ newEntry entry
            caller = functionName $ fromJust $ (stCaller st)
            callee = fromJust $ lookup name (stFunctions st)
            newEntry entry = entry {cgCallees = callee : (cgCallees entry) }

    handleCExpr _ st = st

determineCallGraph :: Context -> CallGraph
determineCallGraph ctx = stCg $ execTrav traverseCTranslUnit (ctxAst ctx) (emptyState ctx)
