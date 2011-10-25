module Ocram.Analysis.CallGraph
-- exports {{{1
(
    CriticalFunctions, BlockingFunctions, StartFunctions
  , call_graph, from_test_graph, to_test_graph
  , blocking_functions, critical_functions, start_functions
  , is_blocking, is_start, is_critical, is_critical_not_blocking
  , function_declaration, function_definition, function_info
  , call_chain, call_order, callees
) where

-- imports {{{1
import Data.Graph.Inductive.Graph (mkGraph)
import Data.Graph.Inductive.Query.BFS (bfs)
import Data.Monoid (mempty)
import Language.C.Data.Ident (Ident(Ident))
import Language.C.Data.Node (NodeInfo, undefNode)
import Language.C.Syntax.AST
import Ocram.Analysis.Types
import Ocram.Analysis.FunctionInfo (decl2fi, def2fi)
import Ocram.Symbols (symbol, Symbol)
import Ocram.Types (Ast)
import Ocram.Util (tmap, fromJust_s)
import Ocram.Visitor (DownVisitor(..), UpVisitor(..), traverseCFunDef, ListVisitor, EmptyDownState, emptyDownState)
import qualified Data.Graph.Inductive.Basic as G
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.Query.BFS as G
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

import Debug.Trace (trace)

-- types {{{1
type CriticalFunctions = Set.Set Symbol

type BlockingFunctions = Set.Set Symbol

type StartFunctions = Set.Set Symbol

call_graph :: Ast -> CallGraph -- {{{1
call_graph ast =
  let 
    labels = createLabels ast
    nodes = createNodes labels
    gi = createGraphIndex nodes
    edges = createEdges gi nodes
    gd = G.mkGraph nodes edges
    cg = CallGraph gd gi
    bf = blocking_functions cg
    cf = getCriticalFunctions cg bf
    gd' = flagCriticalFunctions gd cf
    gd'' = addFunctionInfos gd
  in
    CallGraph gd' gi

from_test_graph :: [(Symbol, Symbol)] -> CallGraph -- {{{1
from_test_graph edges =
  let
    (callers, callees) = unzip edges
    labels = map (\name -> Label name [] Nothing Nothing) $ List.nub $ callers ++ callees
    nodes = createNodes labels
    gi = createGraphIndex nodes
    resolve symbol = gi Map.! symbol
    edges' = map (\e -> ((resolve . fst) e, (resolve . snd) e, undefNode)) edges
    gd = G.mkGraph nodes edges'
  in
    CallGraph gd gi

to_test_graph :: CallGraph -> [(Symbol, Symbol)] -- {{{1
to_test_graph (CallGraph gd _) =
  map (tmap (gnode2symbol gd)) $ G.edges gd

blocking_functions :: CallGraph -> BlockingFunctions -- {{{1
blocking_functions = functionsWith attrBlocking

critical_functions :: CallGraph -> CriticalFunctions -- {{{1
critical_functions = functionsWith attrCritical

start_functions :: CallGraph -> StartFunctions -- {{{1
start_functions = functionsWith attrStart

is_blocking :: CallGraph -> Symbol -> Bool -- {{{1
is_blocking cg name = functionIs attrBlocking cg name

is_critical :: CallGraph -> Symbol -> Bool -- {{{1
is_critical cg name = functionIs attrCritical cg name

is_critical_not_blocking :: CallGraph -> Symbol -> Bool -- {{{1
is_critical_not_blocking cg name = is_critical cg name && not (is_blocking cg name)

is_start :: CallGraph -> Symbol -> Bool -- {{{1
is_start cg name = functionIs attrStart cg name

function_declaration :: CallGraph -> Symbol -> Maybe CDecl -- {{{1
function_declaration (CallGraph gd gi) name = Map.lookup name gi >>= G.lab gd >>= extractFunctionDeclaration

function_definition :: CallGraph -> Symbol -> Maybe CFunDef -- {{{1
function_definition (CallGraph gd gi) name = Map.lookup name gi >>= G.lab gd >>= extractFunctionDefinition

call_chain :: CallGraph -> Symbol -> Symbol -> Maybe [Symbol] -- {{{1
call_chain (CallGraph gd gi) start end = do
  gstart <- Map.lookup start gi
  gend <- Map.lookup end gi
  let path = G.esp gstart gend gd
  return $ map (gnode2symbol gd) path

call_order :: CallGraph -> Symbol -> Maybe [Symbol] -- {{{1
call_order (CallGraph gd gi) start = do
  gstart <- Map.lookup start gi
  return $ map (gnode2symbol gd) $ List.nub $ G.bfs gstart gd

callees :: CallGraph -> Symbol -> Maybe [Symbol] -- {{{1
callees (CallGraph gd gi) caller = do
  gcaller <- Map.lookup caller gi
  return $ map (gnode2symbol gd) $ List.nub $ G.suc gd gcaller
   

function_info :: CallGraph -> Symbol -> Maybe FunctionInfo -- {{{1
function_info cg fname
  | is_blocking cg fname = fmap decl2fi $ function_declaration cg fname
  | is_critical cg fname = fmap def2fi $ function_definition cg fname
  | otherwise = Nothing

-- utils {{{1
gnode2symbol :: GraphData -> G.Node -> Symbol
gnode2symbol gd = lblName . fromJust_s "CallGraph/1" . G.lab gd

createLabels :: Ast -> [Label]
createLabels (CTranslUnit ds _) = foldr processExtDecl [] ds


processExtDecl :: CExtDecl -> [Label] -> [Label]
processExtDecl (CDeclExt x) labels =
  let
    attr = if isBlockingFunction x
      then [Blocking]
      else []
    label = Label (symbol x) attr (Just (FunDecl x)) Nothing
  in
    label : labels

processExtDecl (CFDefExt x) labels =
  let
    attr = if isStartFunction x
      then [Start]
      else []
    label = Label (symbol x) attr (Just (FunDef x)) Nothing
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
processEdge _ es (_, (Label _ _ (Just (FunDecl _)) _)) = es

processEdge gi es (caller, (Label _ _ (Just (FunDef fd)) _)) =
  let
    us = snd $ traverseCFunDef fd emptyDownState
    edges = map (\(callee, ni) -> (caller, gi Map.! callee, ni)) us
  in
    es ++ edges


type CgUpState = [(Symbol, NodeInfo)]

instance UpVisitor EmptyDownState CgUpState where
  upCExpr o@(CCall (CVar (Ident callee _ _) _)  _ ni) _ _ = (o, [(callee, ni)])
  upCExpr o _ u = (o, u)

instance ListVisitor EmptyDownState CgUpState


getCriticalFunctions :: CallGraph -> BlockingFunctions -> CriticalFunctions
getCriticalFunctions cg bf =
  let cg' = CallGraph (G.grev (grData cg)) (grIndex cg) in
  Set.fold (subGraph cg') Set.empty bf


subGraph :: CallGraph -> Symbol -> CriticalFunctions -> CriticalFunctions
subGraph cg end cf = -- call graph has reversed edges
  let
    gi = grIndex cg
    gd = grData cg
    gnode = gi Map.! end
    gnodes = bfs gnode gd
    symbols = map (gnode2symbol gd) gnodes
  in
    Set.union cf $ Set.fromList symbols


flagCriticalFunctions :: GraphData -> CriticalFunctions -> GraphData
flagCriticalFunctions gd cf = G.nmap (flagCriticalFunction cf) gd
  where
    flagCriticalFunction cf label@(Label x attr y z)
      | Set.member x cf = (Label x (Critical : attr) y z)
      | otherwise = label

addFunctionInfos :: GraphData -> GraphData
addFunctionInfos gd = G.nmap addFunctionInfo gd
  where
    addFunctionInfo label =
      case lblAstNode label of
        Nothing -> label
        Just (FunDef fd) -> label {lblFi = Just (def2fi fd)}
        Just (FunDecl fd) -> label {lblFi = Just (decl2fi fd)}

functionsWith :: (Attribute -> Bool) -> CallGraph -> Set.Set Symbol
functionsWith pred cg = Set.fromList $ map lblName $ filter (hasAttr pred) $ map snd $ G.labNodes $ grData cg

hasAttr :: (Attribute -> Bool) -> Label -> Bool
hasAttr pred label = any pred (lblAttr label)

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
      let label = fromJust_s "CallGraph/2" $ G.lab gd gnode
      in any pred (lblAttr label)


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
extractFunctionDefinition (Label _ _ (Just (FunDef x)) _) = Just x
extractFunctionDefinition _ = Nothing


extractFunctionDeclaration :: Label -> Maybe CDecl
extractFunctionDeclaration (Label _ _ (Just (FunDecl x)) _) = Just x
extractFunctionDeclaration _ = Nothing
