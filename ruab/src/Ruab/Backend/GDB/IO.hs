module Ruab.Backend.GDB.IO
-- exports {{{1
(
    GDB, Callback
  , start, stop, send_command
) where

-- [The GDB/MI Interface](http://sourceware.org/gdb/current/onlinedocs/gdb/GDB_002fMI.html#GDB_002fMI)
-- GDB version 7.4

-- whenever an MI command results in an error, we recommend that the frontend refreshes all the information shown in the user interface.
-- it is suggested to just always pass the `--thread' and `--frame' options
--

-- imports {{{1
import Control.Concurrent (forkIO, killThread, ThreadId, MVar, newEmptyMVar, newMVar, tryTakeMVar, putMVar, takeMVar)
import Control.Concurrent.STM (TVar, TChan, TMVar, newEmptyTMVar, newTVarIO, newTChanIO, newEmptyTMVarIO, atomically, takeTMVar, readTVar, writeTVar, writeTChan, readTChan, putTMVar)
import Control.Exception (finally)
import Control.Monad (when, replicateM_)
import Control.Monad.Fix (mfix)
import Data.Maybe (isJust)
import Prelude hiding (catch, interact)
import Ruab.Backend.GDB.Commands (add_token, gdb_exit)
import Ruab.Backend.GDB.Representation (Output(Output), Token, get_token, Command, render_command, parse_output)
import Ruab.Backend.GDB.Output (output_response, output_notification, output_stream, Response, Notification, Stream)
import System.IO (Handle, hSetBuffering, BufferMode(LineBuffering), hPutStr, hWaitForInput, hGetLine, stdout)
import System.Posix.IO (fdToHandle, createPipe)
import System.Process (ProcessHandle, runProcess, waitForProcess)

data GDB = GDB { -- {{{1
-- gdb process {{{2
    gdbProcess        :: ProcessHandle
  , gdbCommandPipe    :: Handle
  , gdbOutputPipe     :: Handle
-- callback
  , gdbCallback       :: Callback
-- threads {{{2
  , gdbCommandThread  :: ThreadId
  , gdbOutputThread   :: ThreadId
  , gdbCurrentJob     :: MVar Job
  , gdbFinished       :: MVar ()
-- jobs
  , gdbNextToken      :: TVar Token
  , gdbJobs           :: TChan Job
}

data Job = Job {
    jobCommand  :: Maybe Command
  , jobResponse :: TMVar Response
  , jobToken    :: Token
  }

type Callback = Either Notification Stream -> IO () -- {{{1

start :: Maybe FilePath -> Callback -> IO GDB -- {{{1
start workdir callback = do
  (commandR,  commandW)  <- createPipe >>= asHandles
  (outputR, outputW) <- createPipe >>= asHandles
  phandle <- runProcess "gdb" ["--interpreter", "mi"]
                 workdir Nothing
                 (Just commandR)
                 (Just outputW)
                 Nothing
  mapM_ (`hSetBuffering` LineBuffering) [commandW, outputR]
  response <- newEmptyTMVarIO
  currentJob <- newMVar (Job Nothing response (-1))
  finished <- newEmptyMVar
  nextToken <- newTVarIO 0
  jobs <- newTChanIO
  gdb <- mfix (\gdb -> do
      itid <- forkIO (handleCommands gdb)
      otid <- forkIO (handleOutput gdb)
      return $ GDB phandle commandW outputR callback itid otid currentJob finished nextToken jobs
    )
  _ <- atomically $ takeTMVar response
  return gdb
  where
  asHandles (f1, f2) = do
    h1 <- fdToHandle f1
    h2 <- fdToHandle f2
    return (h1, h2)

stop :: GDB -> IO () -- {{{1
stop gdb = do
  mapM_ (killThread . ($gdb)) [gdbCommandThread, gdbOutputThread]
  replicateM_ 2 (takeMVar (gdbFinished gdb))
  writeCommand gdb gdb_exit 0
  _ <- waitForProcess (gdbProcess gdb)
  return ()

send_command :: GDB -> Command -> IO Response -- {{{1
send_command gdb command = sendCommand >>= receiveResponse
  where
    sendCommand = atomically $ do
      token <- readTVar (gdbNextToken gdb)
      writeTVar (gdbNextToken gdb) (token + 1)
      response <- newEmptyTMVar
      let job = Job (Just command) response token
      writeTChan (gdbJobs gdb) job
      return response
    
    receiveResponse = atomically . takeTMVar

-- implementation {{{1
handleCommands :: GDB -> IO () -- {{{2
handleCommands gdb = do
  job <- atomically $ readTChan (gdbJobs gdb)
  putMVar (gdbCurrentJob gdb) job
  case jobCommand job of
    Nothing -> return ()
    Just job' -> writeCommand gdb job' (jobToken job)
  handleCommands gdb
  `finally` putMVar (gdbFinished gdb) ()

handleOutput :: GDB -> IO () -- {{{2
handleOutput gdb = do
  output@(Output _ rr) <- readOutput gdb
  let response = output_response output
  _ <- forkIO $ do
    mapM_ (gdbCallback gdb . Left)  (output_notification output)
    mapM_ (gdbCallback gdb . Right) (output_stream output)
  maybJob <- tryTakeMVar (gdbCurrentJob gdb)
  case maybJob of
    Nothing -> when (isJust rr) (error "result record lost!")
    Just job -> atomically $ do
      when (jobToken job /= (-1) && get_token output /= Just (jobToken job)) (error "token missmatch!")
      putTMVar (jobResponse job) response
  handleOutput gdb
  `finally` putMVar (gdbFinished gdb) ()

writeCommand :: GDB -> Command -> Token -> IO () -- {{{2
writeCommand gdb cmd token = 
  let cmdstr = (render_command . add_token token) cmd in
  do
    hPutStr stdout ("// " ++ cmdstr)
    hPutStr (gdbCommandPipe gdb) cmdstr

readOutput :: GDB -> IO Output -- {{{2
readOutput gdb = do
  _ <- hWaitForInput (gdbOutputPipe gdb) (-1)
  str <- outputString (gdbOutputPipe gdb)
  hPutStr stdout ("// " ++ str)
  return (parse_output str)
  where
    outputString handle = outputLines handle >>= return . unlines
    outputLines handle = do
      line <- hGetLine handle
      if line == "(gdb) "
        then return [line]
        else outputLines handle >>= return . (line:)

