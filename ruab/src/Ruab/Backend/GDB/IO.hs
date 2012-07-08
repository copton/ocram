module Ruab.Backend.GDB.IO
-- exports {{{1
(
    Context, Callback
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
import Control.Exception (catchJust)
import Control.Exception.Base (AsyncException(ThreadKilled))
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

data Context = Context { -- {{{1
-- gdb process {{{2
    ctxProcess        :: ProcessHandle
  , ctxCommandPipe    :: Handle
  , ctxOutputPipe     :: Handle
-- callback
  , ctxCallback       :: Callback
-- threads {{{2
  , ctxCommandThread  :: ThreadId
  , ctxOutputThread   :: ThreadId
  , ctxCurrentJob     :: MVar Job
  , ctxFinished       :: MVar ()
-- jobs
  , ctxNextToken      :: TVar Token
  , ctxJobs           :: TChan Job
}

data Job = Job {
    jobCommand  :: Maybe Command
  , jobResponse :: TMVar Response
  , jobToken    :: Token
  }

type Callback = Either Notification Stream -> IO () -- {{{1

start :: Maybe FilePath -> Callback -> IO Context -- {{{1
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
  ctx <- mfix (\ctx -> do
      itid <- forkIO (handleCommands ctx)
      otid <- forkIO (handleOutput ctx)
      return $ Context phandle commandW outputR callback itid otid currentJob finished nextToken jobs
    )
  _ <- atomically $ takeTMVar response
  return ctx
  where
  asHandles (f1, f2) = do
    h1 <- fdToHandle f1
    h2 <- fdToHandle f2
    return (h1, h2)

stop :: Context -> IO () -- {{{1
stop ctx = do
  mapM_ (killThread . ($ctx)) [ctxCommandThread, ctxOutputThread]
  replicateM_ 2 (takeMVar (ctxFinished ctx))
  writeCommand ctx gdb_exit 0
  _ <- waitForProcess (ctxProcess ctx)
  return ()

send_command :: Context -> Command -> IO Response -- {{{1
send_command ctx command = sendCommand >>= receiveResponse
  where
    sendCommand = atomically $ do
      token <- readTVar (ctxNextToken ctx)
      writeTVar (ctxNextToken ctx) (token + 1)
      response <- newEmptyTMVar
      let job = Job (Just command) response token
      writeTChan (ctxJobs ctx) job
      return response
    
    receiveResponse = atomically . takeTMVar

-- implementation {{{1
handleCommands :: Context -> IO () -- {{{2
handleCommands ctx = handleKill ctx $ do
  job <- atomically $ readTChan (ctxJobs ctx)
  putMVar (ctxCurrentJob ctx) job
  case jobCommand job of
    Nothing -> return ()
    Just job' -> writeCommand ctx job' (jobToken job)
  handleCommands ctx

handleOutput :: Context -> IO () -- {{{2
handleOutput ctx = handleKill ctx $ do
  output@(Output _ rr) <- readOutput ctx
  let response = output_response output
  _ <- forkIO $ do
    mapM_ (ctxCallback ctx . Left)  (output_notification output)
    mapM_ (ctxCallback ctx . Right) (output_stream output)
  maybJob <- tryTakeMVar (ctxCurrentJob ctx)
  case maybJob of
    Nothing -> when (isJust rr) (error "result record lost!")
    Just job -> do
      when (jobToken job /= (-1) && get_token output /= Just (jobToken job)) (error "token missmatch!")
      atomically $ putTMVar (jobResponse job) response
  handleOutput ctx

handleKill :: Context -> IO () -> IO ()
handleKill ctx action = catchJust select action handler
  where
    select :: AsyncException -> Maybe ()
    select ThreadKilled = Just ()
    select _ = Nothing

    handler :: () -> IO ()
    handler _ = putMVar (ctxFinished ctx) ()

writeCommand :: Context -> Command -> Token -> IO () -- {{{2
writeCommand ctx cmd token = 
  let cmdstr = (render_command . add_token token) cmd in
  do
    debugLog True cmdstr
    hPutStr (ctxCommandPipe ctx) cmdstr

readOutput :: Context -> IO Output -- {{{2
readOutput ctx = do
  _ <- hWaitForInput (ctxOutputPipe ctx) (-1)
  str <- outputString (ctxOutputPipe ctx)
  debugLog False str
  return (parse_output str)
  where
    outputString handle = outputLines handle >>= return . unlines
    outputLines handle = do
      line <- hGetLine handle
      if line == "(gdb) "
        then return [line]
        else outputLines handle >>= return . (line:)

debugLog :: Bool -> String -> IO () -- {{{2
debugLog io text = let prefix = if io then "/i " else "/o " in
    hPutStr stdout ((unlines . map (prefix++) . lines) text)
