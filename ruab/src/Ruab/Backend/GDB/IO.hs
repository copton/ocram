module Ruab.Backend.GDB.IO
-- exports {{{1
(
    AsyncGDB, Callback
  , start, quit, send_command
  , SyncGDB(async_gdb), sync_start, sync_send_command
  , module Ruab.Backend.GDB.Representation
) where

-- imports {{{1
import Control.Concurrent (forkIO, killThread, ThreadId, MVar, newEmptyMVar, newMVar, tryTakeMVar, putMVar, takeMVar)
import Control.Exception (finally)
import Control.Monad.Fix (mfix)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (fromJust, isNothing)
import Prelude hiding (catch, interact)
import Ruab.Backend.GDB.Commands (add_token, gdb_exit)
import Ruab.Backend.GDB.Representation
import System.IO (Handle, hSetBuffering, BufferMode(LineBuffering), hReady, hPutStr, hWaitForInput, hGetLine, stdout)
import System.Posix.IO (fdToHandle, createPipe)
import System.Process (ProcessHandle, runProcess, terminateProcess, waitForProcess)

data AsyncGDB = AsyncGDB { -- {{{1
    gdbHandle   :: ProcessHandle
  , gdbCommand  :: Handle
  , gdbResponse :: Handle
  , gdbToken    :: IORef Token
  , gdbCallback :: Callback
  , gdbFinished :: MVar ()
  , gdbThread   :: ThreadId
}

type Callback = Output -> IO () -- {{{1

start :: Maybe FilePath -> Callback -> IO AsyncGDB -- {{{1
start workdir callback = do
  (commandR,  commandW)  <- createPipe >>= asHandles
  (responseR, responseW) <- createPipe >>= asHandles
  phandle <- runProcess "gdb" ["--interpreter", "mi"]
                 workdir Nothing
                 (Just commandR)
                 (Just responseW)
                 Nothing
  mapM_ (`hSetBuffering` LineBuffering) [commandW, responseR]
  tokenRef <- newIORef 1
  finished <- newMVar ()
  gdb <- mfix (\gdb -> do
      tid <- forkIO (handleOutput gdb)
      return $ AsyncGDB phandle commandW responseR tokenRef callback finished tid
    )
  return gdb
  where
  asHandles (f1, f2) = do
    h1 <- fdToHandle f1; h2 <- fdToHandle f2; return (h1, h2)

quit :: AsyncGDB -> IO () -- {{{1
quit gdb = do
  killThread (gdbThread gdb)
  _ <- takeMVar (gdbFinished gdb)
  writeCommand gdb gdb_exit 0
  _ <- waitForProcess (gdbHandle gdb)
  return ()

send_command :: AsyncGDB -> Command -> IO Token -- {{{1
send_command gdb command = do
  token <- nextToken gdb
  writeCommand gdb command token
  return token

-- implementation {{{1
handleOutput :: AsyncGDB -> IO () -- {{{2
handleOutput gdb =
  readOutput gdb >>= gdbCallback gdb >> handleOutput gdb
  `finally` putMVar (gdbFinished gdb) ()

nextToken :: AsyncGDB -> IO Token -- {{{2
nextToken gdb = do
  token <- readIORef  (gdbToken gdb)
  writeIORef (gdbToken gdb) (token + 1)
  return token

writeCommand :: AsyncGDB -> Command -> Token -> IO () -- {{{2
writeCommand gdb cmd token = 
  let cmdstr = (render_command . add_token token) cmd in
  do
    hPutStr stdout cmdstr
    hPutStr (gdbCommand gdb) cmdstr

readOutput :: AsyncGDB -> IO Output -- {{{2
readOutput gdb = do
  _ <- hWaitForInput (gdbResponse gdb) (-1)
  str <- outputString (gdbResponse gdb)
  hPutStr stdout str
  return (parse_output str)
  where
    outputString handle = outputLines handle >>= return . unlines
    outputLines handle = do
      line <- hGetLine handle
      if line == "(gdb) "
        then return [line]
        else outputLines handle >>= return . (line:)

-- synchronous {{{1
data SyncGDB = SyncGDB {
    async_gdb    :: AsyncGDB
  , syncFlag     :: MVar ()
  , syncToken    :: MVar Token
  , syncOutput   :: MVar Output
  , syncCallback :: Callback
  }

sync_start :: Maybe FilePath -> Callback -> IO SyncGDB
sync_start workdir callback = do
  flag <- newEmptyMVar
  token <- newEmptyMVar
  output <- newEmptyMVar
  mfix (\sgdb -> do
      let callback' = callbackWrapper sgdb
      agdb <- start workdir callback'
      return $ SyncGDB agdb flag token output callback
    )
   
callbackWrapper :: SyncGDB -> Callback
callbackWrapper sync = \output -> do
  flag <- tryTakeMVar (syncFlag sync)
  case flag >> get_token output of
    Nothing -> (syncCallback sync) output
    Just expected -> do
      existent <- takeMVar (syncToken sync)
      if expected == existent
        then putMVar (syncOutput sync) output
        else (syncCallback sync) output 
      let (Output oobs _) = output
      case filter (flt expected) oobs of
        [] -> return ()
        oobs' -> (syncCallback sync) (Output oobs' Nothing)
  where
    flt expected oob =
      let existent = get_token oob in
      isNothing existent || fromJust existent /= expected

sync_send_command :: SyncGDB -> Command -> IO Output
sync_send_command sgdb command = do
  putMVar (syncFlag sgdb) ()
  token <- send_command (async_gdb sgdb) command
  putMVar (syncToken sgdb) token
  takeMVar (syncOutput sgdb)
