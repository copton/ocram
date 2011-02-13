module Analysis.Algorithms.CallGraph (
     determineCallGraph
) where

import Language.C.Syntax.AST
import Language.C.Data.Ident
import Data.Map (update, fromList, toList, findWithDefault, member)
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
        cg = fromList $ map convert (toList fm)
        convert (k, fd) = (k, Entry fd [] [])

instance Visitor State where
    handleCFunDef fd st = st { stCaller = Just fd }

    handleCExpr (CCall (CVar (Ident name _ _) _)  _ _) st 
        | member name (stFunctions st) = st {stCg = update update' caller (stCg st)}
        | otherwise = st
          where 
            update' entry = Just $ newEntry entry
            caller = functionName $ fromJust $ (stCaller st)
            callee = findWithDefault err name (stFunctions st)
            err = error $ "could not find definition of function " ++ name
            newEntry entry = entry {cgCallees = callee : (cgCallees entry) }

    handleCExpr _ st = st

determineCallGraph :: Context -> CallGraph
determineCallGraph ctx = stCg $ execTrav traverseCTranslUnit (ctxAst ctx) (emptyState ctx)
