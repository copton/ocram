module Ocram.Ruab.Frontend
-- exports {{{1
(
  ruab_ui
) where

import Ocram.Options (Options)
import Ocram.Debug (LocMap, VarMap)

import qualified Data.ByteString.Char8 as BS

ruab_ui :: Options -> BS.ByteString -> BS.ByteString -> VarMap -> LocMap -> IO ()
ruab_ui = undefined
