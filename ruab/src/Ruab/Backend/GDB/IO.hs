module Ruab.Backend.GDB.IO
-- exports {{{1
(
    GDB, Callback
  , start, quit, send_command
) where

-- [The GDB/MI Interface](http://sourceware.org/gdb/current/onlinedocs/gdb/GDB_002fMI.html#GDB_002fMI)
-- GDB version 7.4

-- whenever an MI command results in an error, we recommend that the frontend refreshes all the information shown in the user interface.
-- it is suggested to just always pass the `--thread' and `--frame' options
--

-- imports {{{1
import Control.Concurrent (forkIO, killThread, ThreadId, MVar, newEmptyMVar, newMVar, tryTakeMVar, putMVar, takeMVar)
import Control.Exception (finally)
import Control.Monad.Fix (mfix)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (isJust)
import Prelude hiding (catch, interact)
import Ruab.Backend.GDB.Commands (add_token, gdb_exit)
import Ruab.Backend.GDB.Representation (Output, Token, get_token, Command, render_command, parse_output)
import Ruab.Backend.GDB.Output (response, notification, stream, Response, Notification, Stream)
import System.IO (Handle, hSetBuffering, BufferMode(LineBuffering), hPutStr, hWaitForInput, hGetLine, stdout)
import System.Posix.IO (fdToHandle, createPipe)
import System.Process (ProcessHandle, runProcess, waitForProcess)

data GDB = GDB { -- {{{1
    gdbHandle   :: ProcessHandle
  , gdbCommand  :: Handle
  , gdbOutput   :: Handle
  , gdbCounter  :: IORef Token
  , gdbToken    :: MVar Token
  , gdbResponse :: MVar Response
  , gdbCallback :: Callback
  , gdbFinished :: MVar ()
  , gdbThread   :: ThreadId
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
  counterRef <- newIORef 0
  tokenVar <- newEmptyMVar
  responseVar <- newEmptyMVar
  finished <- newMVar ()
  gdb <- mfix (\gdb -> do
      tid <- forkIO (handleOutput gdb)
      return $ GDB phandle commandW outputR counterRef tokenVar responseVar callback finished tid
    )
  return gdb
  where
  asHandles (f1, f2) = do
    h1 <- fdToHandle f1; h2 <- fdToHandle f2; return (h1, h2)

quit :: GDB -> IO () -- {{{1
quit gdb = do
  killThread (gdbThread gdb)
  _ <- takeMVar (gdbFinished gdb)
  writeCommand gdb gdb_exit 0
  _ <- waitForProcess (gdbHandle gdb)
  return ()

send_command :: GDB -> Command -> IO Response -- {{{1
send_command gdb command = do
  token <- nextToken gdb
  putMVar (gdbToken gdb) token
  writeCommand gdb command token
  takeMVar (gdbResponse gdb)

-- implementation {{{1
handleOutput :: GDB -> IO () -- {{{2
handleOutput gdb = do
  output <- readOutput gdb
  mapM_ (gdbCallback gdb . Left)  (notification output)
  mapM_ (gdbCallback gdb . Right) (stream output)
  token <- tryTakeMVar (gdbToken gdb)
  if isJust token && token == get_token output
    then putMVar (gdbResponse gdb) (response output)
    else return ()
  handleOutput gdb
  `finally` putMVar (gdbFinished gdb) ()

nextToken :: GDB -> IO Token -- {{{2
nextToken gdb = do
  token <- readIORef  (gdbCounter gdb)
  writeIORef (gdbCounter gdb) (token + 1)
  return token

writeCommand :: GDB -> Command -> Token -> IO () -- {{{2
writeCommand gdb cmd token = 
  let cmdstr = (render_command . add_token token) cmd in
  do
    hPutStr stdout cmdstr
    hPutStr (gdbCommand gdb) cmdstr

readOutput :: GDB -> IO Output -- {{{2
readOutput gdb = do
  _ <- hWaitForInput (gdbOutput gdb) (-1)
  str <- outputString (gdbOutput gdb)
  hPutStr stdout str
  return (parse_output str)
  where
    outputString handle = outputLines handle >>= return . unlines
    outputLines handle = do
      line <- hGetLine handle
      if line == "(gdb) "
        then return [line]
        else outputLines handle >>= return . (line:)

