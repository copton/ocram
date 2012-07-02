module Ruab.Backend.GDB
-- exports {{{1
(
    Backend, Callback
  , backend_start, backend_stop
  , set_breakpoint, Location, file_line_location, file_function_location, Breakpoint
  , backend_run
) where

-- imports {{{1
import Prelude hiding (interact)
import Ruab.Backend.GDB.Commands
import Ruab.Backend.GDB.IO (GDB, Callback, start, stop, send_command)
import Ruab.Backend.GDB.Output
import Ruab.Backend.GDB.Responses

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

set_breakpoint :: Backend -> Location -> IO (Either String Breakpoint) -- {{{1
set_breakpoint gdb loc = do
  resp <- send_command gdb (break_insert False False False False False Nothing Nothing Nothing loc)
  return $ convert "break-insert" response_break_insert resp

backend_run :: Backend -> IO ()
backend_run gdb = do
  resp <- send_command gdb (exec_run (Left True))
  if not (is_running resp)
    then error $ "unexpected response for exec-run: '" ++ show resp ++ "'"
    else return ()

-- utils {{{1
convert :: String -> (Dictionary -> Maybe a) -> Response -> Either String a -- {{{2
convert s f resp =
  if is_error resp
    then Left (show resp)
    else case dictionary resp >>= f of
      Nothing -> error $ "invalid response for " ++ s ++ ": '" ++ show resp ++ "'"
      Just x -> Right x

