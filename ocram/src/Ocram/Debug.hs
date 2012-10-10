{-# LANGUAGE TemplateHaskell #-}
module Ocram.Debug (
    Breakpoint(..), Breakpoints
  , VarMap'
  , create_debug_info
  , module Ocram.Debug.Enriched
) where

-- import {{{1
import Data.Digest.OpenSSL.MD5 (md5sum)
import Ocram.Analysis (Analysis(..))
import Ocram.Debug.Enriched
import Ocram.Debug.Internal
import Ocram.Options (Options(optInput, optOutput))
import Ocram.Ruab

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M

create_debug_info :: -- {{{1
     Options       -- command line options
  -> BS.ByteString -- T-code
  -> BS.ByteString -- P-code
  -> Analysis      -- result of analysis
  -> VarMap'       -- result of intermediate
  -> Breakpoints   -- breakpoints
  -> BS.ByteString -- E-code      
  -> DebugInfo
create_debug_info opt tcode pcode ana vm' bps ecode =
  let
    tfile = File (optInput opt) (md5sum tcode)
    efile = File (optOutput opt) (md5sum ecode)
    mtp   = t2p_map tcode pcode
    mpe   = p2e_map mtp bps
    vm    = var_map mtp vm'
    ts    = all_threads (anaCallgraph ana)
    os    = M.keys (anaBlocking ana)
  in 
    DebugInfo tfile pcode efile mtp mpe vm ts os
