{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
module Ocram.Debug where

-- import {{{1
import Data.Data (Data)
import Data.Digest.OpenSSL.MD5 (md5sum)
import Data.Typeable (Typeable)
import Language.C.Data.Node (CNode(nodeInfo), NodeInfo, undefNode)
import Ocram.Analysis (CallGraph, start_functions, blocking_functions, call_order)
import Ocram.Options (Options(optInput, optOutput))
import Ocram.Debug.Internal
import Ocram.Ruab
import Ocram.Transformation.Names (threadExecutionFunction)
import Ocram.Util (fromJust_s)

import qualified Data.ByteString.Char8 as BS

data ENodeInfo = ENodeInfo { -- {{{1
    enTnodeInfo     :: NodeInfo
  , enThreadId      :: Maybe Int
  , enBreakpoint    :: Bool
  , enBlockingCall  :: Bool
  } deriving (Data, Typeable)

instance CNode ENodeInfo where
  nodeInfo = enTnodeInfo

instance Show ENodeInfo where
  show = show . enTnodeInfo

un :: ENodeInfo -- {{{1
un = enrich_node_info undefNode

enrich_node_info :: NodeInfo -> ENodeInfo -- {{{1
enrich_node_info ni = ENodeInfo ni Nothing False False

create_debug_info :: Options -> CallGraph -> BS.ByteString -> BS.ByteString -> BS.ByteString -> VarMap -> Breakpoints -> BlockingCalls -> DebugInfo -- {{{1
create_debug_info opt cg tcode pcode ecode vm lm bkl =
  let
    tfile = File (optInput opt) (md5sum tcode)
    efile = File (optOutput opt) (md5sum ecode)
    ts = zipWith createThreadInfo [0..] (start_functions cg)
    ppm = preproc_map tcode pcode
    oa = blocking_functions cg
  in
    DebugInfo tfile pcode efile ppm lm bkl vm ts oa
  where
    createThreadInfo tid sf = Thread tid sf (threadExecutionFunction tid) ($fromJust_s $ call_order cg sf)
