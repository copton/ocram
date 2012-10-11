{-# LANGUAGE DeriveDataTypeable #-}
module Ocram.Debug.Enriched where

-- imports {{{1
import Data.Data (Data)
import Data.Typeable (Typeable)
import Language.C.Syntax.AST
import Ocram.Ruab (ThreadId)
import Language.C.Data.Node (NodeInfo, CNode(nodeInfo), undefNode, getLastTokenPos, posOfNode, fileOfNode)
import Language.C.Data.Position (posRow)

data ENodeInfo  -- {{{1
  = EnUndefined
  | EnWrapper {
    enNodeInfo       :: NodeInfo
  }
  | EnBreakpoint {
      enThreadId     :: Maybe ThreadId
    , enTRow         :: Int
    , enBlockingCall :: Bool
    , enFile         :: Maybe FilePath
  }
  deriving (Data, Typeable, Show)

instance CNode ENodeInfo where -- {{{2
  nodeInfo (EnWrapper n) = n
  nodeInfo _             = undefNode

eun :: ENodeInfo  -- {{{1
eun = EnUndefined

node_start :: CNode n => n -> ENodeInfo -- {{{1
node_start node = 
  let ni = nodeInfo node in
  EnBreakpoint Nothing ((posRow . posOfNode) ni) False (fileOfNode ni)

node_end :: CNode n => n -> ENodeInfo -- {{{1
node_end node =
  let ni = nodeInfo node in
  EnBreakpoint Nothing ((posRow . fst . getLastTokenPos) ni) False (fileOfNode ni)

aset :: Annotated a => ENodeInfo -> a b -> a ENodeInfo -- {{{1
aset eni = fmap (const eni)

set_thread :: ThreadId -> ENodeInfo -> ENodeInfo -- {{{1
set_thread tid (EnBreakpoint _ r b f) = EnBreakpoint (Just tid) r b f
set_thread _   o                      = o

set_blocking :: ENodeInfo -> ENodeInfo -- {{{1
set_blocking (EnBreakpoint t r _ f) = EnBreakpoint t r True f
set_blocking o                      = o

-- AST -- {{{1
type CTranslUnit' = CTranslationUnit ENodeInfo
type CExpr' = CExpression ENodeInfo
type CBlockItem' = CCompoundBlockItem ENodeInfo
type CStat' = CStatement ENodeInfo
type CFunDef' = CFunctionDef ENodeInfo
type CDesignator' = CPartDesignator ENodeInfo
type CInit' = CInitializer ENodeInfo
type CDecl' = CDeclaration ENodeInfo
type CDeclr' = CDeclarator ENodeInfo
type CExtDecl' = CExternalDeclaration ENodeInfo
