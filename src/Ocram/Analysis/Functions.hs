module Ocram.Analysis.Functions
-- exports {{{1
(
		CallGraph, CriticalFunctions, BlockingFunctions, StartFunctions
	, call_graph, from_test_graph, to_test_graph
	, blocking_functions, critical_functions, start_functions
	, is_blocking, is_start, is_critical
	, get_function_declaration, get_function_definition
	, get_call_chain
) where

-- imports {{{1
import Control.Arrow ((***))
import Data.Graph.Inductive.Graph (mkGraph)
import Data.Graph.Inductive.Query.BFS (bfs)
import Data.Maybe (fromJust)
import Data.Monoid (mempty)
import Language.C.Data.Ident (Ident(Ident))
import Language.C.Syntax.AST
import Ocram.Symbols (symbol, Symbol)
import Ocram.Types (Ast)
import Ocram.Visitor (DownVisitor(..), UpVisitor(..), traverseCFunDef, ListVisitor, EmptyDownState, emptyDownState)
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.PatriciaTree as PT
import qualified Data.Graph.Inductive.Query.BFS as BFS
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

-- types {{{1
data Attribute =
		Blocking
	| Critical
	| Start

data AstNode =
		FunDef CFunDef
	| FunDecl CDecl

data Label = Label {
		lblName :: Symbol
	, lblAttr :: [Attribute]
	, lblAstNode :: Maybe AstNode
}

type Node = (G.Node, Label)

type Edge = (G.Node, G.Node, ())

type GraphData = PT.Gr Label ()

type GraphIndex = Map.Map Symbol G.Node

data CallGraph = CallGraph {
		grData :: GraphData
	, grIndex :: GraphIndex
}

type CriticalFunctions = Set.Set Symbol

type BlockingFunctions = Set.Set Symbol

type StartFunctions = Set.Set Symbol

-- call_graph :: Ast -> CallGraph {{{1
call_graph :: Ast -> CallGraph
call_graph ast =
	let 
		labels = createLabels ast
		nodes = createNodes labels
		gi = createGraphIndex nodes
		edges = createEdges gi nodes
		gd = G.mkGraph nodes edges
		cg = CallGraph gd gi
		sfs = start_functions cg
		cf = getCriticalFunctions cg sfs
		gd' = flagCriticalFunctions gd cf
	in
		CallGraph gd' gi

-- from_test_graph :: [(Symbol, Symbol)] -> CallGraph {{{1
from_test_graph :: [(Symbol, Symbol)] -> CallGraph
from_test_graph edges =
	let
		(callers, callees) = unzip edges
		labels = map (\name -> Label name [] Nothing) $ List.nub $ callers ++ callees
		nodes = createNodes labels
		gi = createGraphIndex nodes
		resolve symbol = gi Map.! symbol
		edges' = map (\e -> ((resolve . fst) e, (resolve . snd) e, ())) edges
		gd = G.mkGraph nodes edges'
	in
		CallGraph gd gi

-- to_test_graph :: CallGraph -> [(Symbol, Symbol)] {{{1
to_test_graph :: CallGraph -> [(Symbol, Symbol)]
to_test_graph (CallGraph gd _) =
	map (tmap convert) $ G.edges gd
	where
		convert gnode = lblName $ fromJust $ G.lab gd gnode


-- blocking_functions :: CallGraph -> BlockingFunctions {{{1
blocking_functions :: CallGraph -> BlockingFunctions
blocking_functions = functionsWith attrBlocking

-- critical_functions :: CallGraph -> CriticalFunctions {{{1
critical_functions :: CallGraph -> CriticalFunctions
critical_functions = functionsWith attrCritical

-- start_functions :: CallGraph -> StartFunctions {{{1
start_functions :: CallGraph -> StartFunctions
start_functions = functionsWith attrStart

-- is_blocking :: CallGraph -> Symbol -> Bool {{{1
is_blocking :: CallGraph -> Symbol -> Bool
is_blocking cg name = functionIs attrBlocking cg name

-- is_critical :: CallGraph -> Symbol -> Bool {{{1
is_critical :: CallGraph -> Symbol -> Bool
is_critical cg name = functionIs attrCritical cg name

-- is_start :: CallGraph -> Symbol -> Bool {{{1
is_start :: CallGraph -> Symbol -> Bool
is_start cg name = functionIs attrStart cg name

-- get_function_declaration :: CallGraph -> Symbol -> Maybe CDecl {{{1
get_function_declaration :: CallGraph -> Symbol -> Maybe CDecl
get_function_declaration (CallGraph gd gi) name = Map.lookup name gi >>= G.lab gd >>= extractFunctionDeclaration

-- get_function_definition :: CallGraph -> Symbol -> Maybe CFunDef {{{1
get_function_definition :: CallGraph -> Symbol -> Maybe CFunDef
get_function_definition (CallGraph gd gi) name = Map.lookup name gi >>= G.lab gd >>= extractFunctionDefinition

-- get_call_chain :: CallGraph -> Symbol -> Symbol -> Maybe [Symbol] {{{1
get_call_chain :: CallGraph -> Symbol -> Symbol -> Maybe [Symbol]
get_call_chain (CallGraph gd gi) start end = do
	gstart <- Map.lookup start gi
	gend <- Map.lookup end gi
	let path = BFS.esp gstart gend gd
	return $ map (lblName . fromJust . (G.lab gd)) path

-- utils {{{1
tmap f = f *** f

createLabels :: Ast -> [Label]
createLabels (CTranslUnit ds _) = foldr processExtDecl [] ds


processExtDecl :: CExtDecl -> [Label] -> [Label]
processExtDecl (CDeclExt x) labels =
	let
		attr = if isBlockingFunction x
			then [Blocking]
			else []
		label = Label (symbol x) attr $ Just $ FunDecl x
	in
		label : labels

processExtDecl (CFDefExt x) labels =
	let
		attr = if isStartFunction x
			then [Start]
			else []
		label = Label (symbol x) attr $ Just $ FunDef x
	in
		 label : labels

processExtDecl _ labels = labels


createNodes :: [Label] -> [Node]
createNodes labels = zip [1..] labels


createGraphIndex :: [Node] -> GraphIndex
createGraphIndex nodes = Map.fromList $ map processNode nodes


processNode :: Node -> (Symbol, G.Node)
processNode node = ((lblName . snd) node, fst node)


createEdges :: GraphIndex -> [Node] -> [Edge]
createEdges gi nodes = foldl (processEdge gi) [] nodes


processEdge :: GraphIndex -> [Edge] -> Node -> [Edge]
processEdge _ es (_, (Label _ _ (Just (FunDecl _)))) = es

processEdge gi es (caller, (Label _ _ (Just (FunDef fd)))) =
	let
		callees = snd $ traverseCFunDef fd emptyDownState
		edges = map (\callee -> (caller, gi Map.! callee, ())) callees
	in
		es ++ edges


type CgUpState = [Symbol]

instance UpVisitor EmptyDownState CgUpState where
	upCExpr o@(CCall (CVar (Ident callee _ _) _)  _ _) _ _ = (o, [callee])
	upCExpr o _ u = (o, u)

instance ListVisitor EmptyDownState CgUpState


getCriticalFunctions :: CallGraph -> StartFunctions -> CriticalFunctions
getCriticalFunctions cg sf = Set.fold (subGraph cg) Set.empty sf


subGraph :: CallGraph -> Symbol -> CriticalFunctions -> CriticalFunctions
subGraph cg start cf =
	let
		gi = grIndex cg
		gd = grData cg
		gnode = gi Map.! start
		gnodes = bfs gnode gd
		labels = map (fromJust . (G.lab gd)) gnodes
		symbols = map lblName labels
	in
		Set.union cf $ Set.fromList symbols


flagCriticalFunctions :: GraphData -> CriticalFunctions -> GraphData
flagCriticalFunctions gd cf = G.nmap (flagCriticalFunction cf) gd


flagCriticalFunction :: CriticalFunctions -> Label -> Label
flagCriticalFunction cf (Label x attr y) = (Label x (Critical : attr) y)


functionsWith :: (Attribute -> Bool) -> CallGraph -> Set.Set Symbol
functionsWith pred cg = Set.fromList $ map lblName $ filter (hasAttr pred) $ map snd $ G.labNodes $ grData cg

hasAttr :: (Attribute -> Bool) -> Label -> Bool
hasAttr pred (Label _ as _) = any pred as

attrBlocking Blocking = True
attrBlocking _  = False
attrStart Start = True
attrStart _ = False
attrCritical Critical = True
attrCritical _ = False


functionIs :: (Attribute -> Bool) -> CallGraph -> Symbol -> Bool
functionIs pred (CallGraph gd gi) name =
	case Map.lookup name gi of
		Nothing -> False
		Just gnode ->
			let (Label _ attr _) = fromJust $ G.lab gd gnode
			in any pred attr


isBlockingFunction :: CDecl -> Bool
isBlockingFunction (CDecl ss [(Just (CDeclr (Just (Ident name _ _)) [CFunDeclr _ _ _] Nothing _ _), Nothing, Nothing)] _) =
	any isBlockingAttribute ss

isBlockingFunction _ = False


isBlockingAttribute (CTypeQual (CAttrQual (CAttr (Ident blockingAttr _ _) [] _))) = True
isBlockingAttribute _ = False


isStartFunction:: CFunDef -> Bool
isStartFunction (CFunDef specs _ _ _ _) = any isStartAttr specs 


isStartAttr (CTypeQual (CAttrQual (CAttr (Ident startAttr _ _) [] _))) = True
isStartAttr _ = False


extractFunctionDefinition :: Label -> Maybe CFunDef
extractFunctionDefinition (Label _ _ (Just (FunDef x))) = Just x
extractFunctionDefinition _ = Nothing


extractFunctionDeclaration :: Label -> Maybe CDecl
extractFunctionDeclaration (Label _ _ (Just (FunDecl x))) = Just x
extractFunctionDeclaration _ = Nothing
