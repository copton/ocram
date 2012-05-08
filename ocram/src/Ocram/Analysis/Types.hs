module Ocram.Analysis.Types where

import Language.C.Data.Node (NodeInfo)
import Ocram.Symbols (Symbol)
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.PatriciaTree as G
import qualified Data.Map as Map

data Attribute =
    Blocking
  | Start
  | Critical

data Label = Label {
    lblName :: Symbol
  , lblAttr :: [Attribute]
}

type Node = (G.Node, Label)

type Edge = (G.Node, G.Node, NodeInfo)

type GraphData = G.Gr Label NodeInfo

type GraphIndex = Map.Map Symbol G.Node

data CallGraph = CallGraph {
    grData :: GraphData
  , grIndex :: GraphIndex
}
