{-# LANGUAGE TemplateHaskell #-}
module Ocram.Analysis.CallGraph
-- exports {{{1
(
    BlockingFunctions, StartFunctions, CriticalFunctions, Footprint
  , call_graph, from_test_graph, to_test_graph
  , blocking_functions, start_functions, critical_functions
  , is_blocking, is_start, is_critical
  , call_chain, call_order, get_callees, get_callers, dependency_list, footprint
  , get_all_callers, get_all_callees
  , to_string
) where

-- imports {{{1
import Control.Arrow (first, second)
import Control.Monad (guard)
import Data.Generics (mkQ, everything)
import Data.Maybe (isJust, maybeToList)
import Data.Tuple (swap)
import Language.C.Data.Ident (Ident(Ident))
import Language.C.Data.Node (undefNode, NodeInfo)
import Language.C.Syntax.AST
import Ocram.Analysis.Types
import Ocram.Query (is_function_declaration, is_blocking_function, is_start_function, function_definition)
import Ocram.Symbols (symbol, Symbol)
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
type NonCriticalFunctions = [Symbol]
type Footprint = [[Symbol]]

call_graph :: CTranslUnit -> CallGraph -- {{{1
call_graph ast =
  let 
    labels = createLabels ast
    nodes  = createNodes labels
    gi     = createGraphIndex nodes
    edges  = createEdges ast gi nodes
    gd     = G.mkGraph nodes edges
    gd'    = updateLabels gd
  in
    CallGraph gd' gi

from_test_graph :: [(Symbol, Symbol)] -> CallGraph -- {{{1
from_test_graph edges =
  let
    (callers, callees) = unzip edges
    nodes = createNodes $ map (\name -> Label name [Critical]) $ List.nub $ callers ++ callees
    gi = createGraphIndex nodes
    resolve = $lookup_s gi
    edges' = map (\e -> ((resolve . fst) e, (resolve . snd) e, undefNode)) edges
    gd = G.mkGraph nodes edges'
  in
    CallGraph gd gi

to_test_graph :: CallGraph -> [(Symbol, Symbol)] -- {{{1
to_test_graph (CallGraph gd _) =
  map (tmap (gnode2symbol gd)) $ G.edges gd

to_string :: CallGraph -> String
to_string (CallGraph gd _) = show (
    map snd (G.labNodes gd)
  , map edge (G.labEdges gd)
  )
  where edge (from, to, _) = map ($fromJust_s . G.lab gd) [from, to]

blocking_functions :: CallGraph -> BlockingFunctions -- {{{1
blocking_functions = functionsWith (any attrBlocking)

start_functions :: CallGraph -> StartFunctions -- {{{1
start_functions = functionsWith (any attrStart)

critical_functions :: CallGraph -> CriticalFunctions -- {{{1
critical_functions = functionsWith (any attrCritical)

non_critical_functions :: CallGraph -> NonCriticalFunctions
non_critical_functions = functionsWith null

is_blocking :: CallGraph -> Symbol -> Bool -- {{{1
is_blocking = functionIs attrBlocking

is_start :: CallGraph -> Symbol -> Bool -- {{{1
is_start = functionIs attrStart

is_critical :: CallGraph -> Symbol -> Bool -- {{{1
is_critical = functionIs attrCritical

call_chain :: CallGraph -> Symbol -> Symbol -> Maybe [Symbol] -- {{{1
call_chain (CallGraph gd gi) start end = do
  gstart <- Map.lookup start gi
  gend <- Map.lookup end gi
  return $ criticalSubset gd $ G.esp gstart gend gd

call_order :: CallGraph -> Symbol -> Maybe [Symbol] -- {{{1
call_order (CallGraph gd gi) start =
  Map.lookup start gi >>= return . List.nub . criticalSubset gd . flip G.bfs gd

dependency_list :: CallGraph -> [Symbol] -- {{{1
dependency_list (CallGraph gd _) = 
  List.nub $ concatMap depList $ nodesByAttr gd attrStart 
  where
    depList = List.reverse . criticalSubset gd . flip G.bfs gd

get_callees :: CallGraph -> Symbol -> [(Symbol, NodeInfo)] -- {{{1
get_callees (CallGraph gd gi) caller = do
  gcaller <- maybeToList $ Map.lookup caller gi
  (_, gcallee, ni) <- G.out gd gcaller
  let label = $fromJust_s $ G.lab gd gcallee
  guard $ (any attrCritical . lblAttr) label
  return $ (lblName label, ni)

get_callers :: CallGraph -> Symbol -> [(Symbol, NodeInfo)] -- {{{1
get_callers (CallGraph gd gi) caller = do
  gcaller <- maybeToList $ Map.lookup caller gi
  (gcallee, _, ni) <- G.inn gd gcaller
  let label = $fromJust_s $ G.lab gd gcallee
  guard $ (any attrCritical . lblAttr) label
  return $ (lblName label, ni)

get_all_callees :: CallGraph -> Symbol -> [(Symbol, NodeInfo)] -- {{{1
get_all_callees (CallGraph gd gi) caller = do
  gcaller <- maybeToList $ Map.lookup caller gi
  (_, gcallee, ni) <- G.out gd gcaller
  return $ (gnode2symbol gd gcallee, ni)

get_all_callers :: CallGraph -> Symbol -> [(Symbol, NodeInfo)] -- {{{1
get_all_callers (CallGraph gd gi) caller = do
  gcaller <- maybeToList $ Map.lookup caller gi
  (gcallee, _, ni) <- G.inn gd gcaller
  return $ (gnode2symbol gd gcallee, ni)
   
footprint :: CallGraph -> Footprint -- {{{1
footprint cg@(CallGraph gd gi) = map footprint' $ start_functions cg
  where
  footprint' sf = filter (is_blocking cg) $ $fromJust_s $ call_order cg sf

-- utils {{{1
gnode2symbol :: GraphData -> G.Node -> Symbol -- {{{2
gnode2symbol gd = lblName . $fromJust_s . G.lab gd

criticalSubset :: GraphData -> [G.Node] -> [Symbol]
criticalSubset gd = 
  map lblName . filter (any attrCritical . lblAttr) . map ($fromJust_s . G.lab gd)

createNodes :: [Label] -> [Node] -- {{{2
createNodes = zip [0..]

createLabels :: CTranslUnit -> [Label] -- {{{2
createLabels (CTranslUnit ds _) = foldr go [] ds
  where
  go (CDeclExt x) labels
    | is_function_declaration x && is_blocking_function x =
      Label (symbol x) [Blocking] : labels
    | otherwise = labels

  go (CFDefExt x) labels =
    let
      attr = [Start | is_start_function x]
      label = Label (symbol x) attr
    in
      label : labels

  go _ labels = labels

updateLabels :: GraphData -> GraphData -- {{{2
updateLabels gd = G.gmap update gd
  where
    update o@(ine, gnode, label, oute)
      | Set.member gnode cn = (ine, gnode, label {lblAttr = Critical : lblAttr label}, oute)
      | otherwise           = o

    cn       = Set.intersection backward forward
    backward = reachableSubGraph (G.grev gd) $ nodesByAttr gd attrBlocking
    forward  = reachableSubGraph gd $ nodesByAttr gd attrStart

reachableSubGraph :: GraphData -> [G.Node] -> Set.Set G.Node -- {{{3
reachableSubGraph gd ns = Set.fromList $ concatMap (flip G.bfs gd) ns

nodesByAttr :: GraphData -> (Attribute -> Bool) -> [G.Node] -- {{{3
nodesByAttr g attr = map fst $ filter (any attr . lblAttr . snd) $ G.labNodes g

createGraphIndex :: [Node] -> GraphIndex -- {{{2
createGraphIndex nodes = Map.fromList $ map (swap . second lblName) nodes

createEdges :: CTranslUnit -> GraphIndex -> [Node] -> [Edge] -- {{{2
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

functionsWith :: ([Attribute] -> Bool) -> CallGraph -> [Symbol] -- {{{2
functionsWith pred cg = map lblName $ filter (pred . lblAttr) $ map snd $ G.labNodes $ grData cg

attrBlocking :: Attribute -> Bool -- {{{2
attrBlocking = (==Blocking)

attrStart :: Attribute -> Bool -- {{{2
attrStart = (==Start)

attrCritical :: Attribute -> Bool -- {{{2
attrCritical = (==Critical)

functionIs :: (Attribute -> Bool) -> CallGraph -> Symbol -> Bool -- {{{2
functionIs pred (CallGraph gd gi) name =
  case Map.lookup name gi of
    Nothing -> False
    Just gnode -> any pred . lblAttr . $fromJust_s $ G.lab gd gnode

