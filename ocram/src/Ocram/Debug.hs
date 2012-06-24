{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
module Ocram.Debug where

-- import {{{1
import Data.Data (Data)
import Data.Digest.OpenSSL.MD5 (md5sum)
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)
import Language.C.Data.Node (lengthOfNode, isUndefNode, posOfNode, CNode(nodeInfo), NodeInfo, undefNode)
import Language.C.Data.Position (posRow, posColumn)
import Ocram.Analysis (CallGraph, start_functions, blocking_functions, call_order)
import Ocram.Options (Options(optInput, optOutput))
import Ocram.Ruab
import Ocram.Util (abort, fromJust_s)

import qualified Data.ByteString.Char8 as BS

data ENodeInfo = ENodeInfo { -- {{{1
  enTnodeInfo :: NodeInfo,
  enThreadId :: Maybe Int,
  enIsBreakpoint :: Bool
  } deriving (Data, Typeable)

instance CNode ENodeInfo where
  nodeInfo = enTnodeInfo

instance Show ENodeInfo where
  show _ = ""

un :: ENodeInfo -- {{{1
un = enrichNodeInfo undefNode

enrichNodeInfo :: NodeInfo -> ENodeInfo -- {{{1
enrichNodeInfo ni = ENodeInfo ni Nothing False

enableBreakpoint :: ENodeInfo -> ENodeInfo -- {{{1
enableBreakpoint eni
  | isUndefNode (enTnodeInfo eni) = $abort "enabling breakpoint for undefined node"
  | otherwise = eni {enIsBreakpoint = True}

validBreakpoint :: ENodeInfo -> Bool -- {{{1
validBreakpoint (ENodeInfo tni _ bp) = bp && not (isUndefNode tni)

setThread :: Int -> ENodeInfo -> ENodeInfo -- {{{1
setThread tid eni = eni {enThreadId = Just tid}

tlocation :: ENodeInfo -> TLocation -- {{{1
tlocation eni =
  let
    ni = enTnodeInfo eni
    pos = posOfNode ni
  in
    TLocation (posRow pos) (posColumn pos) (fromMaybe (-1) (lengthOfNode ni))

create_debug_info :: Options -> CallGraph -> BS.ByteString -> BS.ByteString -> BS.ByteString -> VarMap -> LocMap -> DebugInfo
create_debug_info opt cg tcode pcode ecode vm lm =
  let
    tfile = File (optInput opt) (md5sum tcode)
    efile = File (optOutput opt) (md5sum ecode)
    ts = zipWith createThreadInfo [0..] (start_functions cg)
    oa = blocking_functions cg
  in
    DebugInfo tfile pcode efile lm vm ts oa
  where
    createThreadInfo tid sf = Thread tid sf ($fromJust_s $ call_order cg sf)
