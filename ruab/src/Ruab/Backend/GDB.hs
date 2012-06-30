{-# LANGUAGE TemplateHaskell #-}
module Ruab.Backend.GDB
(
    backend_start
  , backend_quit
  , set_breakpoint
  , Backend
) where

import Data.List (intercalate)
import Prelude hiding (interact)
import Ruab.Backend.GDB.Commands
import Ruab.Backend.GDB.Representation
import Ruab.Backend.GDB.IO
import Ruab.Util (abort, save)

type Backend = SyncGDB

backend_start :: FilePath -> Callback -> IO (Either String Backend)
backend_start binary callback = save $ do
  sgdb <- sync_start Nothing callback
  output <- sync_send_command sgdb (file_exec_and_symbols (Just binary))
  if is_error output
    then callback output
    else return ()
  return sgdb

backend_quit :: Backend -> IO ()
backend_quit = quit . async_gdb

set_breakpoint :: Backend -> FilePath -> Int -> IO (Either String Token)
set_breakpoint gdb file line = save $ send_command (async_gdb gdb) (break_insert False False False False False Nothing Nothing Nothing (file_line_location file line))
