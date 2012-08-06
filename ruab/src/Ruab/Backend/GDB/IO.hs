module Ruab.Backend.GDB.IO
-- exports {{{1
(
    Context, Callback(..)
  , setup, shutdown, send_command
) where

-- [The GDB/MI Interface](http://sourceware.org/gdb/current/onlinedocs/gdb/GDB_002fMI.html#GDB_002fMI)
-- GDB version 7.4

-- whenever an MI command results in an error, we recommend that the frontend refreshes all the information shown in the user interface.
-- it is suggested to just always pass the `--thread' and `--frame' options
--

-- imports {{{1
import Control.Applicative ((<*>), (<$>))
import Control.Concurrent (forkIO, killThread, ThreadId, MVar, newEmptyMVar, tryTakeMVar, putMVar, takeMVar)
import Control.Concurrent.STM (TVar, TChan, TMVar, newEmptyTMVar, newTVarIO, newTChanIO, atomically, takeTMVar, readTVar, writeTVar, writeTChan, readTChan, putTMVar)
import Control.Exception (catchJust)
import Control.Exception.Base (AsyncException(ThreadKilled))
import Control.Monad (replicateM_)
import Control.Monad.Fix (mfix)
import Data.List (partition)
import Prelude hiding (catch, interact)
import System.IO (Handle, hSetBuffering, BufferMode(LineBuffering), hPutStr, hWaitForInput, hGetLine, IOMode(WriteMode), stdout, openFile, hFlush)
import System.Posix.IO (fdToHandle, createPipe)
import System.Process (ProcessHandle, runProcess, waitForProcess)

import qualified Ruab.Backend.GDB.Commands as C
import qualified Ruab.Backend.GDB.Representation as R
import qualified Ruab.Backend.GDB.Responses as S

data Context = Context { -- {{{1
-- gdb process {{{2
    ctxProcess        :: ProcessHandle
  , ctxCommandPipe    :: Handle
  , ctxOutputPipe     :: Handle
  , ctxLog            :: Maybe Handle
-- callback
  , ctxCallback       :: Callback
-- threads
  , ctxCommandThread  :: ThreadId
  , ctxOutputThread   :: ThreadId
  , ctxCurrentJob     :: MVar Job
  , ctxFinished       :: MVar ()
-- jobs
  , ctxNextToken      :: TVar R.Token
  , ctxJobs           :: TChan Job
}

data Job = Job {
    jobCommand  :: R.Command
  , jobResponse :: TMVar R.Response
  , jobToken    :: R.Token
  }

data Callback  -- {{{1
  = Callback {
      cbStream  :: R.Stream -> IO ()
    , cbStopped :: S.Stopped -> IO ()
    , cbNotify  :: R.Notification -> IO ()
  }

setup :: Maybe FilePath -> Callback -> IO Context -- {{{1
setup logfile callback = do
  (commandR,  commandW)  <- createPipe >>= asHandles
  (outputR, outputW) <- createPipe >>= asHandles
  phandle <- runProcess "setsid" ["gdb", "--interpreter", "mi"] -- avoid receiving SIGINTs when issuing -exec-interrupt
                 Nothing Nothing
                 (Just commandR)
                 (Just outputW)
                 Nothing
  mapM_ (`hSetBuffering` LineBuffering) [commandW, outputR]
  logH <- case logfile of
    Nothing -> return Nothing
    Just "-" -> return $ Just stdout
    Just f -> fmap Just $ openFile f WriteMode
  currentJob <- newEmptyMVar
  finished <- newEmptyMVar
  nextToken <- newTVarIO 0
  jobs <- newTChanIO
  ctx <- mfix (\ctx -> do
      itid <- forkIO (handleCommands ctx)
      otid <- forkIO (handleOutput ctx)
      return $ Context phandle commandW outputR logH callback itid otid currentJob finished nextToken jobs
    )
  return ctx
  where
  asHandles (f1, f2) = do
    h1 <- fdToHandle f1
    h2 <- fdToHandle f2
    return (h1, h2)

shutdown :: Context -> IO () -- {{{1
shutdown ctx = do
  mapM_ (killThread . ($ctx)) [ctxCommandThread, ctxOutputThread]
  replicateM_ 2 (takeMVar (ctxFinished ctx))
  writeCommand ctx C.gdb_exit 0
  _ <- waitForProcess (ctxProcess ctx)
  putMVar (ctxFinished ctx) ()
  return ()

send_command :: Context -> R.Command -> IO R.Response -- {{{1
send_command ctx command = checkShutdown >> sendCommand >>= receiveResponse
  where
    checkShutdown = do
      finished <- tryTakeMVar (ctxFinished ctx)
      case finished of
        Nothing -> return ()
        Just () -> error "context has already been shut down"

    sendCommand = atomically $ do
      token <- readTVar (ctxNextToken ctx)
      writeTVar (ctxNextToken ctx) (if token == maxBound then 0 else token + 1)
      response <- newEmptyTMVar
      writeTChan (ctxJobs ctx) $ Job command response token
      return response
    
    receiveResponse = atomically . takeTMVar


-- implementation {{{1
handleCommands :: Context -> IO () -- {{{2
handleCommands ctx = handleKill ctx $ do
  job <- atomically $ readTChan (ctxJobs ctx)
  putMVar (ctxCurrentJob ctx) job
  writeCommand ctx (jobCommand job) (jobToken job)
  handleCommands ctx

handleOutput :: Context -> IO () -- {{{2
handleOutput ctx = handleKill ctx $ do
  output  <- readOutput ctx
  _ <- forkIO $
    let
      streams = R.output_stream output
      notifications = R.output_notification output
      (stops, others) = partition ((&&) <$> (R.Exec==) . R.notiClass <*> (R.ACStop==) . R.notiAsyncClass) notifications
      Just stops' = sequence $ map (S.response_stopped . R.notiResults) stops
    in do
      mapM_ ((cbStream . ctxCallback) ctx)  streams
      mapM_ ((cbNotify . ctxCallback) ctx)  others
      mapM_ ((cbStopped . ctxCallback) ctx) stops'
  case R.output_response output of
    Nothing -> return ()
    Just response -> do
      maybJob <- tryTakeMVar (ctxCurrentJob ctx)
      case maybJob of
        Nothing -> error "result record lost!"
        Just job -> 
          if (R.get_token output /= Just (jobToken job))
            then error $ "token missmatch! " ++ show (R.get_token output) ++ " vs. " ++ show (jobToken job)
            else atomically $ putTMVar (jobResponse job) response
  handleOutput ctx  

handleKill :: Context -> IO () -> IO ()
handleKill ctx action = catchJust select action handler
  where
    select :: AsyncException -> Maybe ()
    select ThreadKilled = Just ()
    select _ = Nothing

    handler :: () -> IO ()
    handler _ = putMVar (ctxFinished ctx) ()

writeCommand :: Context -> R.Command -> R.Token -> IO () -- {{{2
writeCommand ctx cmd token = 
  let cmdstr = (R.render_command . C.add_token token) cmd in
  do
    debugLog ctx True cmdstr
    hPutStr (ctxCommandPipe ctx) cmdstr

readOutput :: Context -> IO R.Output -- {{{2
readOutput ctx = do
  _ <- hWaitForInput (ctxOutputPipe ctx) (-1)
  str <- outputString (ctxOutputPipe ctx)
  debugLog ctx False str
  return (R.parse_output str)
  where
    outputString handle = outputLines handle >>= return . unlines
    outputLines handle = do
      line <- hGetLine handle
      if line == "(gdb) "
        then return [line]
        else outputLines handle >>= return . (line:)

debugLog :: Context -> Bool -> String -> IO () -- {{{2
debugLog ctx io text = 
  let
    prefix = if io then "/i " else "/o "
    line = ((unlines . map (prefix++) . lines) text)
  in
  case (ctxLog ctx) of
    Nothing -> return ()
    Just h -> hPutStr h line >> hFlush h
