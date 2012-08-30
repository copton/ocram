{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
module Ocram.Debug where

-- import {{{1
import Data.Data (Data)
import Data.Digest.OpenSSL.MD5 (md5sum)
import Data.Typeable (Typeable)
import Language.C.Data.Node (CNode(nodeInfo), NodeInfo, undefNode)
import Ocram.Analysis (CallGraph, start_functions, blocking_functions, call_order, critical_functions)
import Ocram.Options (Options(optInput, optOutput))
import Ocram.Debug.Internal
import Ocram.Ruab
import Ocram.Symbols (Symbol)
import Ocram.Transformation.Names (threadExecutionFunction)
import Ocram.Util (fromJust_s)

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M

data Substitution = Substitution {
    substTVar :: Symbol
  , substEVar :: Symbol
  , substFunc :: Symbol
  } deriving (Data, Typeable, Show)

data ENodeInfo = ENodeInfo { -- {{{1
    enTnodeInfo     :: NodeInfo
  , enThreadId      :: Maybe Int
  , enLocation      :: Bool
  , enBlockingCall  :: Bool
  , enSubst         :: Maybe Substitution
  } deriving (Data, Typeable, Show)

instance CNode ENodeInfo where
  nodeInfo = enTnodeInfo

un :: ENodeInfo -- {{{1
un = enrich_node_info undefNode

enrich_node_info :: NodeInfo -> ENodeInfo -- {{{1
enrich_node_info ni = ENodeInfo ni Nothing False False Nothing

data Location = Location { -- {{{1
    bpTloc           :: TLocation
  , bpEloc           :: ELocation
  , bpThreadId       :: Maybe ThreadId
  } deriving (Show)

type Locations = [Location] -- {{{1

create_debug_info :: Options -> CallGraph -> BS.ByteString -> BS.ByteString -> BS.ByteString -> VarMap -> Locations -> BlockingCalls -> DebugInfo -- {{{1
create_debug_info opt cg tcode pcode ecode vm bps bcs =
  let
    tfile = File (optInput opt) (md5sum tcode)
    efile = File (optOutput opt) (md5sum ecode)
    ts = zipWith createThreadInfo [0..] (start_functions cg)
    ppm = preproc_map tcode pcode
    oa = blocking_functions cg
    lm = createLocMap bps
    cf = critical_functions cg
  in
    DebugInfo tfile pcode efile ppm lm bcs vm ts oa cf
  where
    createThreadInfo tid sf = Thread tid sf (threadExecutionFunction tid) ($fromJust_s $ call_order cg sf)

    createLocMap = LocMap . foldr insert M.empty
    insert bp = M.alter (alter (bpEloc bp)) $ LocKey (bpThreadId bp) (bpTloc bp)
    alter eloc Nothing = Just [eloc]
    alter eloc (Just elocs) = Just $ eloc : elocs
