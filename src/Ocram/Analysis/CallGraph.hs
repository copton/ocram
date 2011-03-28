module Ocram.Analysis.CallGraph (
	 determineCallGraph
) where

import Ocram.Analysis.Types (CallGraph, Entry(Entry), cgCallees, cgCallers, FunctionMap, BlockingFunctions)
import Ocram.Types (Result, AST)
import Ocram.Visitor (DownVisitor(..), UpVisitor(..), traverseCTranslUnit)
import Ocram.Symbols (symbol)
import Language.C.Syntax.AST (CFunDef, CExpression(CCall, CVar))
import Language.C.Data.Ident (Ident(Ident))
import qualified Data.Map as Map
import qualified Data.Set as Set

determineCallGraph :: AST -> FunctionMap -> BlockingFunctions -> Result CallGraph
determineCallGraph ast fm bf = return $ createCallGraph $ snd $ traverseCTranslUnit ast (initDownState fm bf)

data DownState = DownState {
	stCaller :: Maybe CFunDef
, stFunctionMap :: FunctionMap
, stBlockingFunctions :: BlockingFunctions
}

initDownState fm bf = DownState Nothing fm bf

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
	
