{-# LANGUAGE TemplateHaskell #-}
module Ocram.Analysis.CallGraph
-- exports {{{1
(
    CriticalFunctions, BlockingFunctions, StartFunctions, Footprint
  , call_graph, from_test_graph, to_test_graph
  , blocking_functions, critical_functions, start_functions
  , is_blocking, is_start, is_critical
  , call_chain, call_order, get_callees
  , critical_function_dependency_list
  , footprint
) where

-- imports {{{1
import Control.Arrow (first, second)
import Data.Generics (mkQ, everything)
import Data.Maybe (isJust)
import Data.Tuple (swap)
import Language.C.Data.Ident (Ident(Ident))
import Language.C.Data.Node (undefNode)
import Language.C.Syntax.AST
import Ocram.Analysis.Fgl (filter_nodes)
import Ocram.Analysis.Types
import Ocram.Query (is_function_declaration, is_blocking_function', is_start_function', function_definition)
import Ocram.Symbols (symbol, Symbol)
import Ocram.Types (Ast)
import Ocram.Util (tmap, fromJust_s, lookup_s)
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

type Footprint = [[Symbol]]

call_graph :: Ast -> CallGraph -- {{{1
call_graph ast =
  let 
    labels = createLabels ast
    nodes = createNodes labels
    gi = createGraphIndex nodes
    edges = createEdges ast gi nodes
    gd = G.mkGraph nodes edges
    cf = reachableSubGraph (G.grev gd) $ nodesByAttr gd attrBlocking
    rf = reachableSubGraph gd $ nodesByAttr gd attrStart
    cg = CallGraph gd gi
  in
    pruneGraph cg (Set.intersection cf rf)

nodesByAttr :: GraphData -> (Attribute -> Bool) -> [G.Node]
nodesByAttr g attr = map fst $ filter (any attr . lblAttr . snd) $ G.labNodes g

reachableSubGraph :: GraphData -> [G.Node] -> Set.Set G.Node
reachableSubGraph gd ns = Set.fromList $ concatMap (flip G.bfs gd) ns

pruneGraph :: CallGraph -> Set.Set G.Node -> CallGraph
pruneGraph (CallGraph gd gi) ns = 
  let
    gd' = filter_nodes (flip Set.member ns) gd
    gi' = Map.filter (flip Set.member ns) gi
  in
    CallGraph gd' gi'

from_test_graph :: [(Symbol, Symbol)] -> CallGraph -- {{{1
from_test_graph edges =
  let
    (callers, callees) = unzip edges
    nodes = createNodes $ map (\name -> Label name []) $ List.nub $ callers ++ callees
    gi = createGraphIndex nodes
    resolve = $lookup_s gi
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
critical_functions (CallGraph gd _) = Set.fromList $ map (lblName . snd) $ G.labNodes gd

critical_function_dependency_list :: CallGraph -> [Symbol] -- {{{1
critical_function_dependency_list cg@(CallGraph gd gi) = 
  List.nub $ concatMap depList $ Set.toList $ start_functions cg
  where
    depList start = List.reverse $ filter (is_critical cg) $ map (gnode2symbol gd) $ G.bfs ($lookup_s gi start) gd

start_functions :: CallGraph -> StartFunctions -- {{{1
start_functions = functionsWith attrStart

is_blocking :: CallGraph -> Symbol -> Bool -- {{{1
is_blocking = functionIs attrBlocking

is_critical :: CallGraph -> Symbol -> Bool -- {{{1
is_critical _ _ = True

is_start :: CallGraph -> Symbol -> Bool -- {{{1
is_start = functionIs attrStart

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
   
footprint :: CallGraph -> Footprint -- {{{1
footprint cg@(CallGraph gd gi) = map footprint' (Set.toList $ start_functions cg)
  where
  footprint' sf = filter isBlocking $ $fromJust_s $ call_order cg sf
  isBlocking f = hasAttr attrBlocking $ $fromJust_s . G.lab gd $ $lookup_s gi f

-- utils {{{1
gnode2symbol :: GraphData -> G.Node -> Symbol
gnode2symbol gd = lblName . $fromJust_s . G.lab gd

createNodes :: [Label] -> [Node]
createNodes = zip [0..]

createLabels :: Ast -> [Label]
createLabels (CTranslUnit ds _) = foldr go [] ds
  where
  go (CDeclExt x) labels
    | is_function_declaration x && is_blocking_function' x =
      Label (symbol x) [Blocking] : labels
    | otherwise = labels

  go (CFDefExt x) labels =
    let
      attr = [Start | is_start_function' x]
      label = Label (symbol x) attr
    in
      label : labels

  go _ labels = labels

createGraphIndex :: [Node] -> GraphIndex
createGraphIndex nodes = Map.fromList $ map (swap . second lblName) nodes

createEdges :: Ast -> GraphIndex -> [Node] -> [Edge]
createEdges ast gi nodes = foldl go [] nodes
  where
    go es (gcaller, Label caller _) =
      case function_definition ast caller of
        Nothing -> es
        Just fd ->
          let
            calls = filter (isJust . fst) $ map (first (flip Map.lookup gi)) $ criticalCalls fd
            edges = map (\(x, y) -> (gcaller, $fromJust_s x, y)) calls
          in
            es ++ edges
       where
          criticalCalls = everything (++) (mkQ [] criticalCall)
          criticalCall (CCall (CVar (Ident callee _ _) _)  _ ni) = [(callee, ni)]
          criticalCall _ = []


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

functionIs :: (Attribute -> Bool) -> CallGraph -> Symbol -> Bool
functionIs pred (CallGraph gd gi) name =
  case Map.lookup name gi of
    Nothing -> False
    Just gnode -> any pred . lblAttr . $fromJust_s $ G.lab gd gnode
