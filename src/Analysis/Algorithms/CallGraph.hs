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
import Analysis.Types.FunctionMap (functionId, functionId', FunctionMap, FunctionId)
import Context (Context, ctxAst, ctxFunctionMap)

data State = State {
    stCaller :: Maybe FunctionId, 
    stCg :: CallGraph,
    stFunctions :: FunctionMap
}

emptyState ctx = State Nothing cg fm
    where
        fm = ctxFunctionMap ctx
        cg = mapMaybe convert fm
        convert _ = Just $ Entry [] []

instance Visitor State where
    handleCFunDef fd st = st { stCaller = Just $ functionId' fd }

    handleCExpr (CCall (CVar (Ident name _ _) _)  _ _) st 
        | member (functionId name) (stFunctions st) = st {stCg = update update' caller (stCg st)}
        | otherwise = st
          where 
            update' entry = Just $ newEntry entry
            caller = fromJust $ (stCaller st)
            callee = functionId name
            newEntry entry = entry {cgCallees = callee : (cgCallees entry) }

    handleCExpr _ st = st

determineCallGraph :: Context -> CallGraph
determineCallGraph ctx = stCg $ execTrav traverseCTranslUnit (ctxAst ctx) (emptyState ctx)
