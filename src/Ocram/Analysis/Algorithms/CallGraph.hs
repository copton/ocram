module Ocram.Analysis.Algorithms.CallGraph (
	 determineCallGraph
) where

import Language.C.Syntax.AST
import Language.C.Data.Ident
import Data.Map (update, lookup, member, mapMaybe)
import Data.Set (insert, empty)
import Prelude hiding (lookup)
import Data.Maybe (fromJust)
import Ocram.Util.Names (functionName)
import Ocram.Visitor
import Ocram.Analysis.Types.CallGraph
import Ocram.Analysis.Types.FunctionMap (functionId, functionId', FunctionMap, FunctionId)
import Ocram.Context (Context, ctxAst, ctxFunctionMap)

data State = State {
	stCaller :: Maybe FunctionId, 
	stCg :: CallGraph,
	stFunctions :: FunctionMap
}

emptyState ctx = State Nothing cg fm
	where
		fm = ctxFunctionMap ctx
		cg = mapMaybe convert fm
		convert _ = Just $ Entry empty empty

instance Visitor State where
	handleCFunDef fd st = st { stCaller = Just $ functionId' fd }

	handleCExpr (CCall (CVar (Ident name _ _) _)  _ _) st 
		| member (functionId name) (stFunctions st) = st {stCg = update' (stCg st)}
		| otherwise = st
		  where 
			update' cg = update updateCaller callee $ update updateCallee caller cg
			updateCallee entry = Just $ entry { cgCallees = insert callee (cgCallees entry) }
			updateCaller entry = Just $ entry { cgCallers = insert caller (cgCallers entry) }
			caller = fromJust $ (stCaller st)
			callee = functionId name

	handleCExpr _ st = st

determineCallGraph :: Context -> CallGraph
determineCallGraph ctx = stCg $ execTrav traverseCTranslUnit (ctxAst ctx) (emptyState ctx)
