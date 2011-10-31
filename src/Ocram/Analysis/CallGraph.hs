{-# LANGUAGE TemplateHaskell #-}
module Ocram.Analysis.CallGraph
-- exports {{{1
(
    CriticalFunctions, BlockingFunctions, StartFunctions
  , call_graph, from_test_graph, to_test_graph
  , blocking_functions, critical_functions, start_functions
  , is_blocking, is_start, is_critical, is_critical_not_blocking
  , call_chain, call_order, get_callees
) where

-- imports {{{1
import Data.Graph.Inductive.Query.BFS (bfs)
import Language.C.Data.Ident (Ident(Ident))
import Language.C.Data.Node (NodeInfo, undefNode)
import Language.C.Syntax.AST
import Ocram.Analysis.Types
import Ocram.Query (is_blocking_function', is_start_function', function_definition)
import Ocram.Symbols (symbol, Symbol)
import Ocram.Types (Ast)
import Ocram.Util (tmap, fromJust_s, lookup_s)
import Ocram.Visitor (DownVisitor(..), UpVisitor(..), traverseCFunDef, ListVisitor)
import Prelude hiding (pred)
import qualified Data.Graph.Inductive.Basic as G
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.Query.BFS as G
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

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
    edges = createEdges ast gi nodes
    gd = G.mkGraph nodes edges
    cg = CallGraph gd gi
    bf = blocking_functions cg
    cf = getCriticalFunctions cg bf
    gd' = flagCriticalFunctions gd cf
  in
    CallGraph gd' gi

from_test_graph :: [(Symbol, Symbol)] -> CallGraph -- {{{1
from_test_graph edges =
  let
    (callers, callees) = unzip edges
    labels = map (\name -> Label name []) $ List.nub $ callers ++ callees
    nodes = createNodes labels
    gi = createGraphIndex nodes
    resolve sym = $lookup_s gi sym
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

get_callees :: CallGraph -> Symbol -> Maybe [Symbol] -- {{{1
get_callees (CallGraph gd gi) caller = do
  gcaller <- Map.lookup caller gi
  return $ map (gnode2symbol gd) $ List.nub $ G.suc gd gcaller
   

-- utils {{{1
gnode2symbol :: GraphData -> G.Node -> Symbol
gnode2symbol gd = lblName . $fromJust_s . G.lab gd

createLabels :: Ast -> [Label]
createLabels (CTranslUnit ds _) = foldr processExtDecl [] ds

processExtDecl :: CExtDecl -> [Label] -> [Label]
processExtDecl (CDeclExt x) labels =
  let
    attr = if is_blocking_function' x
      then [Blocking]
      else []
    label = Label (symbol x) attr
  in
    label : labels

processExtDecl (CFDefExt x) labels =
  let
    attr = if is_start_function' x
      then [Start]
      else []
    label = Label (symbol x) attr
  in
     label : labels
processExtDecl _ labels = labels

createNodes :: [Label] -> [Node]
createNodes labels = zip [1..] labels

createGraphIndex :: [Node] -> GraphIndex
createGraphIndex nodes = Map.fromList $ map processNode nodes
  where
    processNode node = ((lblName . snd) node, fst node)

createEdges :: Ast -> GraphIndex -> [Node] -> [Edge]
createEdges ast gi nodes = foldl processEdge [] nodes
  where
    processEdge es (gcaller, (Label caller _)) =
      case function_definition ast caller of
        Nothing -> es
        Just fd ->
          es ++ (map createEdge $ snd $ traverseCFunDef fd CgDownState)
       where
          createEdge (callee, ni) = (gcaller, $lookup_s gi callee, ni)

type CgUpState = [(Symbol, NodeInfo)]
data CgDownState = CgDownState

instance DownVisitor CgDownState

instance UpVisitor CgDownState CgUpState where
  upCExpr o@(CCall (CVar (Ident callee _ _) _)  _ ni) _ _ = (o, [(callee, ni)])
  upCExpr o _ u = (o, u)

instance ListVisitor CgDownState CgUpState

getCriticalFunctions :: CallGraph -> BlockingFunctions -> CriticalFunctions
getCriticalFunctions cg bf =
  let cg' = CallGraph (G.grev (grData cg)) (grIndex cg) in
  Set.fold (subGraph cg') Set.empty bf

subGraph :: CallGraph -> Symbol -> CriticalFunctions -> CriticalFunctions
subGraph cg end cf = -- call graph has reversed edges
  let
    gi = grIndex cg
    gd = grData cg
    gnode = $lookup_s gi end
    gnodes = bfs gnode gd
    symbols = map (gnode2symbol gd) gnodes
  in
    Set.union cf $ Set.fromList symbols

flagCriticalFunctions :: GraphData -> CriticalFunctions -> GraphData
flagCriticalFunctions gd cf = G.nmap (flagCriticalFunction cf) gd

flagCriticalFunction :: CriticalFunctions -> Label -> Label
flagCriticalFunction cf label@(Label x attr)
  | Set.member x cf = (Label x (Critical : attr))
  | otherwise = label

functionsWith :: (Attribute -> Bool) -> CallGraph -> Set.Set Symbol
functionsWith pred cg = Set.fromList $ map lblName $ filter (hasAttr pred) $ map snd $ G.labNodes $ grData cg

hasAttr :: (Attribute -> Bool) -> Label -> Bool
hasAttr pred (Label _ as) = any pred as

attrBlocking :: Attribute -> Bool
attrBlocking Blocking = True
attrBlocking _  = False

attrStart :: Attribute -> Bool
attrStart Start = True
attrStart _ = False

attrCritical :: Attribute -> Bool
attrCritical Critical = True
attrCritical _ = False

functionIs :: (Attribute -> Bool) -> CallGraph -> Symbol -> Bool
functionIs pred (CallGraph gd gi) name =
  case Map.lookup name gi of
    Nothing -> False
    Just gnode ->
      let (Label _ attr) = $fromJust_s $ G.lab gd gnode
      in any pred attr
