{-# LANGUAGE TemplateHaskell #-}
module Ruab.Backend.GDB
-- exports {{{1
(
    G.Context, G.Callback(..)
  , setup, shutdown, run
  , G.Location, G.file_line_location, G.file_function_location
  , G.Breakpoint(..), G.Stack(..), G.Frame(..), G.Stopped(..), G.StopReason(..), G.BkptNumber
  , set_breakpoint, remove_breakpoints, continue, step, next, finish, interrupt, backtrace, evaluate_expression
  , G.Notification(..), G.NotificationClass(..), G.Stream(..), G.StreamClass(..), G.AsyncClass(..)
  , G.asConst
) where

-- imports {{{1
import Control.Monad (when, guard)
import Prelude hiding (interact)
import Ruab.Util (abort)

import qualified Gdbmi.Commands       as G
import qualified Gdbmi.IO             as G
import qualified Gdbmi.Semantics      as G
import qualified Gdbmi.Representation as G

setup :: Maybe FilePath -> FilePath -> G.Callback -> IO G.Context -- {{{1
setup logfile binary callback = do
  let config = G.Config (words "schroot -c quantal -p -- gdb") logfile
  ctx <- G.setup config callback

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

remove_breakpoints :: G.Context -> [G.BkptNumber] -> IO () -- {{{1
remove_breakpoints _   []   = return ()
remove_breakpoints ctx bids = do
  resp <- G.send_command ctx (G.break_delete bids)
  when (G.respClass resp /= G.RCDone)
    ($abort $ "unexpected response: " ++ show resp)

evaluate_expression :: G.Context -> String -> IO (Either String String) -- {{{1
evaluate_expression ctx expr = do
  resp <- G.send_command ctx (G.data_evaluate_expression expr)
  case G.respClass resp of
    G.RCDone -> maybe
      ($abort ("unexpected response: " ++ show resp))
      (return . Right)
      (convert G.response_data_evaluate_expression resp)
    
    G.RCError -> maybe
      ($abort ("unexpected response: " ++ show resp))
      (return . Left)
      ((G.response_error . G.respResults) resp)

    _ -> $abort $ "unexpected response: " ++ show resp

run :: G.Context -> IO () -- {{{1
run ctx = do
  resp <- G.send_command ctx (G.exec_run (Left True))
  when (G.respClass resp /= G.RCRunning) 
    ($abort $ "unexpected response: " ++ show resp)

continue :: G.Context -> IO () -- {{{1
continue ctx = do
  resp <- G.send_command ctx (G.exec_continue False (Left True))
  when (G.respClass resp /= G.RCRunning)
    ($abort $ "unexpected response: " ++ show resp)

step :: G.Context -> IO () -- {{{1
step ctx = do
  resp <- G.send_command ctx G.exec_step
  when (G.respClass resp /= G.RCRunning)
    ($abort $ "unexpected response: " ++ show resp)

next :: G.Context -> IO () -- {{{1
next ctx = do
  resp <- G.send_command ctx G.exec_next
  when (G.respClass resp /= G.RCRunning)
    ($abort $ "unexpected response: " ++ show resp)

finish :: G.Context -> IO () -- {{{1
finish ctx = do
  resp <- G.send_command ctx (G.exec_finish False)
  when (G.respClass resp /= G.RCRunning)
    ($abort $ "unexpected response: " ++ show resp)

backtrace :: G.Context -> IO G.Stack -- {{{1
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
