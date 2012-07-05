module Ruab.Backend.GDB
-- exports {{{1
(
    G.Context, G.Callback
  , start, stop, run
  , G.Location, G.file_line_location, G.file_function_location, G.Breakpoint(..)
  , set_breakpoint, continue_execution
  , G.Notification(..), G.NotifcationType(..), G.Stream(..), G.StreamType(..), G.Event(..)
  , G.asConst
) where

-- imports {{{1
import Control.Monad (when)
import Prelude hiding (interact)

import qualified Ruab.Backend.GDB.Commands as G
import qualified Ruab.Backend.GDB.IO as G
import qualified Ruab.Backend.GDB.Output as G
import qualified Ruab.Backend.GDB.Responses as G
import qualified Ruab.Backend.GDB.Representation as G

start :: FilePath -> G.Callback -> IO G.Context -- {{{1
start binary callback = do
  ctx <- G.start Nothing callback
  res <- G.send_command ctx (G.CLICommand Nothing "tty /dev/null") -- http://sourceware.org/bugzilla/show_bug.cgi?id=8759
  when (G.is_error res) (fail (show res))
  res'<- G.send_command ctx (G.file_exec_and_symbols (Just binary))
  when (G.is_error res') (fail (show res'))
  return ctx

stop :: G.Context -> IO () -- {{{1
stop = G.stop

set_breakpoint :: G.Context -> G.Location -> IO (Either String G.Breakpoint) -- {{{1
set_breakpoint ctx loc = do
  resp <- G.send_command ctx (G.break_insert False False False False False Nothing Nothing Nothing loc)
  return $ convert "break-insert" G.response_break_insert resp

run :: G.Context -> IO ()
run ctx = do
  resp <- G.send_command ctx (G.exec_run (Left True))
  if not (G.is_running resp)
    then error $ "unexpected response for exec-run: '" ++ show resp ++ "'"
    else return ()

continue_execution :: G.Context -> IO ()
continue_execution ctx = do
  resp <- G.send_command ctx (G.exec_continue False (Left True))
  if not (G.is_running resp)
    then error $ "unexpected response for exec-continue: '" ++ show resp ++ "'"
    else return ()

-- utils {{{1
convert :: String -> (G.Dictionary -> Maybe a) -> G.Response -> Either String a -- {{{2
convert s f resp =
  if G.is_error resp
    then Left (show resp)
    else case G.dictionary resp >>= f of
      Nothing -> error $ "invalid response for " ++ s ++ ": '" ++ show resp ++ "'"
      Just x -> Right x

