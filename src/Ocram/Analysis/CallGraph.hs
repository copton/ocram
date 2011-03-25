module Ocram.Analysis.CallGraph (
	 determineCallGraph
) where

import Ocram.Analysis.Types (CallGraph, Entry(Entry), cgCallees, cgCallers, FunctionMap, BlockingFunctions)
import Ocram.Visitor (DownVisitor(..), UpVisitor(..), traverseCTranslUnit)
import Ocram.Context (Context, ctxInputAst, ctxFunctionMap, ctxBlockingFunctions)
import Ocram.Symbols (symbol)
import Language.C.Syntax.AST (CFunDef, CExpression(CCall, CVar))
import Language.C.Data.Ident (Ident(Ident))

import qualified Data.Map as Map
import qualified Data.Set as Set

data DownState = DownState {
	stCaller :: Maybe CFunDef
, stFunctionMap :: FunctionMap
, stBlockingFunctions :: BlockingFunctions
}

initDownState :: Context -> DownState
initDownState ctx = DownState Nothing (ctxFunctionMap ctx) (ctxBlockingFunctions ctx)

type Calls = [(CFunDef, String)]

instance DownVisitor DownState where
	downCFunDef fd d = d {stCaller = Just fd}

instance UpVisitor DownState Calls where
	upCExpr (CCall (CVar (Ident callee _ _) _)  _ _) (DownState (Just caller) fm bf) _ 
		| Map.member callee fm = [(caller, callee)]
		| Map.member callee bf = [(caller, callee)]
		| otherwise = []
	upCExpr _ _ _ = []

createCallGraph :: Calls -> CallGraph
createCallGraph calls = foldl addCall Map.empty calls

addCall :: CallGraph -> (CFunDef, String) -> CallGraph
addCall cg (fd, name) = Map.alter addCallee caller $ Map.alter addCaller callee cg
	where 
		caller = symbol fd
		callee = symbol name
		addCaller Nothing = Just $ Entry (Set.singleton caller) Set.empty
		addCaller (Just entry) = Just $ entry { cgCallers = caller `Set.insert` (cgCallers entry) }
		addCallee Nothing = Just $ Entry Set.empty (Set.singleton callee)
		addCallee (Just entry) = Just $ entry { cgCallees = callee `Set.insert` (cgCallees entry) }
	
determineCallGraph :: Context -> CallGraph
determineCallGraph ctx = createCallGraph $ snd $ traverseCTranslUnit (ctxInputAst ctx) (initDownState ctx)
