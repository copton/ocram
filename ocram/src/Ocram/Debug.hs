{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
module Ocram.Debug where

-- import {{{1
import Data.Data (Data)
import Data.Digest.OpenSSL.MD5 (md5sum)
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)
import Language.C.Data.Node (lengthOfNode, posOfNode, CNode(nodeInfo), NodeInfo, undefNode)
import Language.C.Data.Position (posRow, posColumn)
import Ocram.Analysis (CallGraph, start_functions, blocking_functions, call_order)
import Ocram.Options (Options(optInput, optOutput))
import Ocram.Debug.Internal
import Ocram.Ruab
import Ocram.Util (fromJust_s)

import qualified Data.ByteString.Char8 as BS

data ENodeInfo = ENodeInfo { -- {{{1
    enTnodeInfo :: NodeInfo
  , enThreadId :: Maybe Int
  , enTraceLocation :: Bool
  } deriving (Data, Typeable)

instance CNode ENodeInfo where
  nodeInfo = enTnodeInfo

instance Show ENodeInfo where
  show _ = ""

un :: ENodeInfo -- {{{1
un = enrich_node_info undefNode

enrich_node_info :: NodeInfo -> ENodeInfo -- {{{1
enrich_node_info ni = ENodeInfo ni Nothing False

setThread :: Int -> ENodeInfo -> ENodeInfo -- {{{1
setThread tid eni = eni {enThreadId = Just tid}

tlocation :: ENodeInfo -> TLocation -- {{{1
tlocation eni =
  let
    ni = enTnodeInfo eni
    pos = posOfNode ni
  in
    TLocation (posRow pos) (posColumn pos) (fromMaybe (-1) (lengthOfNode ni))

create_debug_info :: Options -> CallGraph -> BS.ByteString -> BS.ByteString -> BS.ByteString -> VarMap -> LocMap -> DebugInfo -- {{{1
create_debug_info opt cg tcode pcode ecode vm lm =
  let
    tfile = File (optInput opt) (md5sum tcode)
    efile = File (optOutput opt) (md5sum ecode)
    ts = zipWith createThreadInfo [0..] (start_functions cg)
    ppm = preproc_map pcode
    oa = blocking_functions cg
    lm' = map (\(tl, el) -> (adjustTloc ppm tl, el)) lm
  in
    DebugInfo tfile pcode efile ppm lm' vm ts oa
  where
    createThreadInfo tid sf = Thread tid sf ($fromJust_s $ call_order cg sf)
    adjustTloc ppm tl = tl {tlocRow = $fromJust_s (map_preprocessed_row ppm (tlocRow tl))}


