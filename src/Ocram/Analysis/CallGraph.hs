module Ocram.Analysis.CallGraph 
-- exports {{{1
(
	 determineCallGraph
) where

-- imports {{{1
import Ocram.Types
import Ocram.Visitor (DownVisitor(..), UpVisitor(..), traverseCTranslUnit, ListVisitor)
import Ocram.Symbols (symbol)
import Language.C.Syntax.AST (CFunDef, CExpression(CCall, CVar))
import Language.C.Data.Ident (Ident(Ident))
import qualified Data.Map as Map
import qualified Data.Set as Set

-- determineCallGraph :: Context -> Result CallGraph {{{1
determineCallGraph :: Context -> Result CallGraph
determineCallGraph ctx = do
	ast <- getSaneAst ctx
	df <- getDefinedFunctions ctx
	bf <- getBlockingFunctions ctx
	return $ createCallGraph $ snd $ traverseCTranslUnit (getAst ast) (initDownState df bf)

data DownState = DownState {
	stCaller :: Maybe CFunDef
, stDefinedFunctions :: DefinedFunctions
, stBlockingFunctions :: BlockingFunctions
}

initDownState fm bf = DownState Nothing fm bf

type Calls = [(CFunDef, String)]

instance DownVisitor DownState where
	downCFunDef fd d = d {stCaller = Just fd}

instance UpVisitor DownState Calls where
	upCExpr o@(CCall (CVar (Ident callee _ _) _)  _ _) (DownState (Just caller) fm bf) _ 
		| Set.member callee fm = (o, [(caller, callee)])
		| Set.member callee bf = (o, [(caller, callee)])
		| otherwise = (o, [])
	upCExpr o _ _ = (o, [])

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
	
instance ListVisitor DownState Calls
