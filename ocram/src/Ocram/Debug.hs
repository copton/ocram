{-# LANGUAGE TemplateHaskell #-}
module Ocram.Debug (
  create_debug_info
-- this module cannot be a facade for its submodules due to dependency cycles.
-- Import the following submodule directly instead:
--  - Ocram.Debug.Enriched
--  - Ocram.Debug.Breakpoint
--  - Ocram.Debug.VarMap
) where

-- import {{{1
import Data.Digest.OpenSSL.MD5 (md5sum)
import Ocram.Analysis (Analysis(..))
import Ocram.Debug.DebugInfo
import Ocram.Options (Options(optInput, optOutput))
import Ocram.Intermediate (Function)
import Ocram.Debug.Types (Breakpoints, VarMap')
import Ocram.Symbols (Symbol)
import Ocram.Ruab (DebugInfo(..), File(..))

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M

create_debug_info :: -- {{{1
     Options       
  -> TCode
  -> PCode
  -> Analysis      
  -> M.Map Symbol Function 
  -> VarMap'      
  -> Breakpoints
  -> ECode
  -> DebugInfo
create_debug_info opt tcode pcode ana cfs vm' bps ecode =
  let
    tfile = File (optInput opt) (md5sum tcode)
    efile = File (optOutput opt) (md5sum ecode)
    mtp   = t2p_map tcode pcode
    mpe   = p2e_map mtp (optInput opt) bps
    vm    = var_map mtp vm'
    sm    = step_map mtp ana cfs 
    ts    = all_threads (anaCallgraph ana)
    os    = M.keys (anaBlocking ana)
  in 
    DebugInfo tfile pcode efile mtp mpe vm sm ts os

type TCode = BS.ByteString
type PCode = BS.ByteString
type ECode = BS.ByteString
