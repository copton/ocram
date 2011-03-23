module Ocram.Analysis.Algorithms.CallGraph (
	 determineCallGraph
) where

import Ocram.Analysis.Types.CallGraph (CallGraph, Entry(Entry), cgCallees, cgCallers)
import Ocram.Visitor (DownVisitor(..), UpVisitor(..), traverseCTranslUnit)
import Ocram.Analysis.Types.FunctionMap (FunctionMap)
import Ocram.Context (Context, ctxInputAst, ctxFunctionMap)
import Ocram.Symbols (symbol)
import Language.C.Syntax.AST (CFunDef, CExpression(CCall, CVar))
import Language.C.Data.Ident (Ident(Ident))

import qualified Data.Map as Map
import qualified Data.Set as Set

newtype DownState = DownState {
	stCaller :: Maybe CFunDef
}

initDownState :: DownState
initDownState = DownState Nothing

type Calls = [(CFunDef, String)]

instance DownVisitor DownState where
	downCFunDef fd _ = DownState $ Just fd

instance UpVisitor DownState Calls where
	upCExpr (CCall (CVar (Ident callee _ _) _)  _ _) (DownState (Just caller)) _ = [(caller, callee)]
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
determineCallGraph ctx = createCallGraph $ snd $ traverseCTranslUnit (ctxInputAst ctx) initDownState
