module Ruab.Backend
-- exports {{{1
(
    module Ruab.Backend.GDB
  , setup
) where

-- imports {{{1
import Ruab.Backend.GDB hiding (setup)
import qualified Ruab.Backend.GDB as B
import Ruab.Options (Options(optGdbLog, optBinary))

setup :: Options -> Callback -> IO Context
setup opt callback = B.setup (optGdbLog opt) (optBinary opt) callback
