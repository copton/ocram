{-# LANGUAGE TemplateHaskell #-}
module Ruab.Backend.GDB
-- exports {{{1
(
    G.Context, G.Callback(..)
  , setup, shutdown, run
  , G.Location, G.file_line_location, G.file_function_location
  , G.Breakpoint(..), G.Stack(..), G.Frame(..), G.Stopped(..), G.StopReason(..), G.BkptNumber
  , set_breakpoint, continue, interrupt, backtrace
  , G.Notification(..), G.NotificationClass(..), G.Stream(..), G.StreamClass(..), G.AsyncClass(..)
  , G.asConst
) where

-- imports {{{1
import Control.Monad (when, guard)
import Prelude hiding (interact)
import Ruab.Util (abort)

import qualified Ruab.Backend.GDB.Commands as G
import qualified Ruab.Backend.GDB.IO as G
import qualified Ruab.Backend.GDB.Responses as G
import qualified Ruab.Backend.GDB.Representation as G

setup :: Maybe FilePath -> FilePath -> G.Callback -> IO G.Context -- {{{1
setup logfile binary callback = do
  ctx <- G.setup logfile callback

  resp <- G.send_command ctx (G.CLICommand Nothing "tty /dev/null") -- http://sourceware.org/bugzilla/show_bug.cgi?id=8759
  when (G.respClass resp /= G.RCDone)
    ($abort ("unexpected response: " ++ show resp))

  resp' <- G.send_command ctx (G.file_exec_and_symbols (Just binary))
  when (G.respClass resp /= G.RCDone)
    ($abort ("unexpected response: " ++ show resp'))
  return ctx

shutdown :: G.Context -> IO () -- {{{1
shutdown = G.shutdown

interrupt :: G.Context -> IO () -- {{{1
interrupt ctx = do
  resp <- G.send_command ctx (G.exec_interrupt (Left True))
  when (G.respClass resp /= G.RCDone)
    ($abort $ "unexpected response: " ++ show resp)

set_breakpoint :: G.Context -> G.Location -> IO G.Breakpoint -- {{{1
set_breakpoint ctx loc = do
  resp <- G.send_command ctx (G.break_insert False False False False False Nothing Nothing Nothing loc)
  maybe
    ($abort ("unexpected response: " ++ show resp))
    return
    (convert G.response_break_insert resp)

run :: G.Context -> IO ()
run ctx = do
  resp <- G.send_command ctx (G.exec_run (Left True))
  when (G.respClass resp /= G.RCRunning) 
    ($abort $ "unexpected response: " ++ show resp)

continue :: G.Context -> IO ()
continue ctx = do
  resp <- G.send_command ctx (G.exec_continue False (Left True))
  when (G.respClass resp /= G.RCRunning)
    ($abort $ "unexpected response: " ++ show resp)

backtrace :: G.Context -> IO G.Stack
backtrace ctx = do
  resp <- G.send_command ctx (G.stack_list_frames Nothing)
  maybe
    ($abort ("unexpected response: " ++ show resp))
    return
    (convert G.response_stack_list_frames resp)

-- utils {{{1
convert :: ([G.Result] -> Maybe a) -> G.Response -> Maybe a -- {{{2
convert f resp = do
  guard (G.respClass resp /= G.RCError)
  f (G.respResults resp)
