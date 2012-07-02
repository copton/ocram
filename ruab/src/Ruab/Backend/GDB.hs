module Ruab.Backend.GDB
(
    Backend, Callback
  , backend_start, backend_stop
  , set_breakpoint
  , module Ruab.Backend.GDB.Output
) where

-- imports {{{1
import Prelude hiding (interact)
import Ruab.Backend.GDB.Commands
import Ruab.Backend.GDB.IO (GDB, Callback, start, stop, send_command)
import Ruab.Backend.GDB.Output

type Backend = GDB -- {{{1

backend_start :: FilePath -> Callback -> IO Backend -- {{{1
backend_start binary callback = do
  gdb <- start Nothing callback
  res <- send_command gdb (file_exec_and_symbols (Just binary))
  if is_error res
    then fail (show res)
    else return gdb

backend_stop:: Backend -> IO () -- {{{1
backend_stop= stop

set_breakpoint :: Backend -> FilePath -> Int -> IO Response -- {{{1
set_breakpoint gdb file line =
  send_command gdb (break_insert False False False False False Nothing Nothing Nothing (file_line_location file line))
