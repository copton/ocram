{-# LANGUAGE DeriveDataTypeable #-}
module Ocram.Debug where

-- import {{{1
import Data.Data (Data)
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)
import Language.C.Data.Node (lengthOfNode, isUndefNode, posOfNode, CNode(nodeInfo), NodeInfo, undefNode)
import Language.C.Data.Position (posRow, posColumn)
import Language.C.Syntax.AST
import Ocram.Symbols (Symbol)
import Ocram.Util (abort)

data File = -- {{{1
  File {fileName :: String, fileChecksum :: String}


data TLocation = -- {{{1
  TLocation {tlocRow :: Int, tlocCol :: Int, tlocLen :: Int}

instance Show TLocation where
  show (TLocation r c l) = show (r, c, l)

data ELocation = -- {{{1
  ELocation {elocRow :: Int, elocCol :: Int, elocTidd :: Maybe Int}

instance Show ELocation where
  show (ELocation r c t) = show (r, c, t)

data Location = Location TLocation ELocation -- {{{1

type LocMap = [Location] -- {{{1

type Variable = Symbol -- {{{1
type VarMap = [(Variable, Variable)]

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

enableBreakpoint :: ENodeInfo -> ENodeInfo -- {{{1
enableBreakpoint eni
  | isUndefNode (tnodeInfo eni) = $abort "enabling breakpoint for undefined node"
  | otherwise = eni {isBreakpoint = True}

validBreakpoint :: ENodeInfo -> Bool -- {{{1
validBreakpoint (ENodeInfo tni _ bp) = bp && not (isUndefNode tni)

setThread :: Int -> ENodeInfo -> ENodeInfo -- {{{1
setThread tid eni = eni {threadId = Just tid}

tlocation :: ENodeInfo -> TLocation -- {{{1
tlocation eni =
  let
    ni = tnodeInfo eni
    pos = posOfNode ni
  in
    TLocation (posRow pos) (posColumn pos) (fromMaybe (-1) (lengthOfNode ni))


-- {{{1 Types
type CTranslUnit' = CTranslationUnit ENodeInfo
type CExpr' = CExpression ENodeInfo
type CBlockItem' = CCompoundBlockItem ENodeInfo
type CStat' = CStatement ENodeInfo
type CFunDef' = CFunctionDef ENodeInfo
type CDesignator' = CPartDesignator ENodeInfo
type CInit' = CInitializer ENodeInfo
type CDecl' = CDeclaration ENodeInfo
