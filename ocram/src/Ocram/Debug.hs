{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
module Ocram.Debug where

-- import {{{1
import Data.Data (Data)
import Data.Digest.OpenSSL.MD5 (md5sum)
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)
import Language.C.Data.Node (lengthOfNode, isUndefNode, posOfNode, CNode(nodeInfo), NodeInfo, undefNode)
import Language.C.Data.Position (posRow, posColumn)
import Ocram.Options (Options(optInput, optOutput))
import Ocram.Ruab
import Ocram.Util (abort)

import qualified Data.ByteString.Char8 as BS

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

create_debug_info :: Options -> BS.ByteString -> BS.ByteString -> BS.ByteString -> VarMap -> LocMap -> DebugInfo
create_debug_info opt tcode pcode ecode vm lm =
  let
    tfile = File (optInput opt) (md5sum tcode)
    efile = File (optOutput opt) (md5sum ecode)
  in
    DebugInfo tfile pcode efile lm vm
