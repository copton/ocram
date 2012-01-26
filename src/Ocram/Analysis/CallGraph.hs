{-# LANGUAGE TemplateHaskell #-}
module Ocram.Analysis.CallGraph
-- exports {{{1
(
    BlockingFunctions, StartFunctions, Footprint
  , call_graph, from_test_graph, to_test_graph
  , blocking_functions, start_functions, critical_functions
  , is_blocking, is_start, is_critical
  , call_chain, call_order, get_callees
  , dependency_list
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
type BlockingFunctions = [Symbol]
type StartFunctions = [Symbol]
type CriticalFunctions = [Symbol]

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

start_functions :: CallGraph -> StartFunctions -- {{{1
start_functions = functionsWith attrStart

critical_functions :: CallGraph -> CriticalFunctions -- {{{1
critical_functions (CallGraph gd _) = map (lblName . snd) $ G.labNodes gd

is_blocking :: CallGraph -> Symbol -> Bool -- {{{1
is_blocking = functionIs attrBlocking

is_start :: CallGraph -> Symbol -> Bool -- {{{1
is_start = functionIs attrStart

is_critical :: CallGraph -> Symbol -> Bool -- {{{1
is_critical (CallGraph _ gi) fname = Map.member fname gi

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

dependency_list :: CallGraph -> [Symbol] -- {{{1
dependency_list cg@(CallGraph gd gi) = 
  List.nub $ concatMap depList $ start_functions cg
  where
    depList start = List.reverse $ map (gnode2symbol gd) $ G.bfs ($lookup_s gi start) gd
get_callees :: CallGraph -> Symbol -> Maybe [Symbol] -- {{{1
get_callees (CallGraph gd gi) caller = do
  gcaller <- Map.lookup caller gi
  return $ map (gnode2symbol gd) $ List.nub $ G.suc gd gcaller
   
footprint :: CallGraph -> Footprint -- {{{1
footprint cg@(CallGraph gd gi) = map footprint' $ start_functions cg
  where
  footprint' sf = filter (is_blocking cg) $ $fromJust_s $ call_order cg sf

-- utils {{{1
gnode2symbol :: GraphData -> G.Node -> Symbol -- {{{2
gnode2symbol gd = lblName . $fromJust_s . G.lab gd

nodesByAttr :: GraphData -> (Attribute -> Bool) -> [G.Node] -- {{{2
nodesByAttr g attr = map fst $ filter (any attr . lblAttr . snd) $ G.labNodes g

reachableSubGraph :: GraphData -> [G.Node] -> Set.Set G.Node -- {{{2
reachableSubGraph gd ns = Set.fromList $ concatMap (flip G.bfs gd) ns

pruneGraph :: CallGraph -> Set.Set G.Node -> CallGraph -- {{{2
pruneGraph (CallGraph gd gi) ns = 
  let
    gd' = filter_nodes (flip Set.member ns) gd
    gi' = Map.filter (flip Set.member ns) gi
  in
    CallGraph gd' gi'

createNodes :: [Label] -> [Node] -- {{{2
createNodes = zip [0..]

createLabels :: Ast -> [Label] -- {{{2
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

createGraphIndex :: [Node] -> GraphIndex -- {{{2
createGraphIndex nodes = Map.fromList $ map (swap . second lblName) nodes

createEdges :: Ast -> GraphIndex -> [Node] -> [Edge] -- {{{2
createEdges ast gi nodes = foldl go [] nodes
  where
    go es (gcaller, Label caller _) =
      case function_definition ast caller of
        Nothing -> es
        Just fd ->
          let
            calls = filter (isJust . fst) $ map (first (flip Map.lookup gi)) $ everything (++) (mkQ [] call) fd
            edges = map (\(x, y) -> (gcaller, $fromJust_s x, y)) calls
          in
            es ++ edges
       
    call (CCall (CVar (Ident callee _ _) _)  _ ni) = [(callee, ni)]
    call _ = []

functionsWith :: (Attribute -> Bool) -> CallGraph -> [Symbol] -- {{{2
functionsWith pred cg = map lblName $ filter (hasAttr pred) $ map snd $ G.labNodes $ grData cg

hasAttr :: (Attribute -> Bool) -> Label -> Bool -- {{{2
hasAttr pred (Label _ as) = any pred as

attrBlocking :: Attribute -> Bool -- {{{2
attrBlocking Blocking = True
attrBlocking _  = False

attrStart :: Attribute -> Bool -- {{{2
attrStart Start = True
attrStart _ = False

functionIs :: (Attribute -> Bool) -> CallGraph -> Symbol -> Bool -- {{{2
functionIs pred (CallGraph gd gi) name =
  case Map.lookup name gi of
    Nothing -> False
    Just gnode -> any pred . lblAttr . $fromJust_s $ G.lab gd gnode

