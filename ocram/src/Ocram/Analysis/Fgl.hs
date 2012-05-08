module Ocram.Analysis.Fgl
-- export {{{1
(
  edge_label, find_loop, filter_nodes
) where

-- import {{{1
import Data.Maybe (isJust)
import Control.Monad (join)
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.List as List

edge_label :: G.Graph gr => gr a b -> G.Node -> G.Node -> [b] -- {{{1
edge_label graph from to =
  map (\(_, _, label) -> label) $ filter (\(f, t, _) -> f == from && t == to) $ G.labEdges graph


find_loop :: G.Graph gr => gr a b -> G.Node -> Maybe [G.Node] -- {{{1
find_loop graph start = findLoop [] start
  where
    findLoop call_stack current
      | current `List.elem` call_stack = Just $ List.reverse $ current : call_stack
      | otherwise = join $ List.find isJust $ map recurse $ G.out graph current
      where
        recurse (_, next, _) = findLoop (current : call_stack) next

filter_nodes :: G.Graph gr => (G.Node -> Bool) -> gr a b -> gr a b -- {{{1
filter_nodes p g = flip G.delNodes g $ filter (not . p) $ G.nodes g
