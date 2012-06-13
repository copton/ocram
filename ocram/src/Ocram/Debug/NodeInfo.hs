{-# LANGUAGE DeriveDataTypeable #-}
module Ocram.Debug.NodeInfo where

-- import {{{1 
import Data.Data (Data)
import Data.Typeable (Typeable)
import Language.C.Data.Node (CNode(..), NodeInfo, undefNode)
import Language.C.Syntax.AST

data ENodeInfo = ENodeInfo { -- {{{1
  tnodeInfo :: NodeInfo,
  threadId :: Maybe Int,
  isBreakpoint :: Bool
  } deriving (Data, Typeable)

instance CNode ENodeInfo where
  nodeInfo = tnodeInfo

instance Show ENodeInfo where
  show _ = ""

un :: ENodeInfo -- {{{1
un = enrichNodeInfo undefNode

enrichNodeInfo :: NodeInfo -> ENodeInfo -- {{{1
enrichNodeInfo ni = ENodeInfo ni Nothing False

-- {{{1 Types
type CTranslUnit' = CTranslationUnit ENodeInfo
type CExpr' = CExpression ENodeInfo
type CBlockItem' = CCompoundBlockItem ENodeInfo
type CStat' = CStatement ENodeInfo
type CFunDef' = CFunctionDef ENodeInfo
type CDesignator' = CPartDesignator ENodeInfo
type CInit' = CInitializer ENodeInfo
type CDecl' = CDeclaration ENodeInfo
