{-# LANGUAGE DeriveDataTypeable #-}
module Ocram.Debug.Enriched where

-- imports {{{1
import Data.Data (Data)
import Data.Typeable (Typeable)
import Language.C.Syntax.AST
import Ocram.Ruab (ThreadId)
import Language.C.Data.Node (CNode(nodeInfo), NodeInfo)

data ENodeInfo = ENodeInfo { -- {{{1
    enTnodeInfo     :: NodeInfo
  , enThreadId      :: Maybe ThreadId
  , enBreakpoint    :: Bool
  , enBlockingCall  :: Bool
  } deriving (Data, Typeable, Show)

instance CNode ENodeInfo where -- {{{2
  nodeInfo = enTnodeInfo

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
