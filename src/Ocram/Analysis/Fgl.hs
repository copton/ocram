module Ocram.Analysis.Fgl
-- export {{{1
(
	edge_label, find_loop
) where

-- import {{{1
import Data.Maybe (isJust)
import Control.Monad (join)
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.List as List

-- edgeLabel :: G.Graph a b -> G.Node -> G.Node -> [b] {{{1
edge_label :: G.Graph gr => gr a b -> G.Node -> G.Node -> [b]
edge_label graph from to =
	map (\(_, _, label) -> label) $ filter (\(f, t, _) -> f == from && t == to) $ G.labEdges graph


-- find_loop :: G.Graph gr => gr a b -> G.Node a -> Maybe [G.Node a] {{{1
find_loop :: G.Graph gr => gr a b -> G.Node -> Maybe [G.Node]
find_loop graph start = findLoop graph [] start


findLoop graph call_stack start
	| List.elem start call_stack = Just $ List.reverse $ start : call_stack
	| otherwise = join $ List.find isJust $ map recurse $ G.out graph start 
  where
    recurse (_, next, _) = findLoop graph (start : call_stack) next
