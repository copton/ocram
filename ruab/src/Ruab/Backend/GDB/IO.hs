module Ruab.Backend.GDB.IO
(
    GDB
  , start, quit, kill
  , interact, poll
  , module Ruab.Backend.GDB.Representation
) where

-- imports {{{1
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Control.Exception (IOException, catch)
import Prelude hiding (catch, interact)
import Ruab.Backend.GDB.Commands (add_token)
import System.Posix.IO (fdToHandle, createPipe)
import System.IO (Handle, hSetBuffering, BufferMode(LineBuffering), hReady, hPutStr, hWaitForInput, hGetLine, stdout)
import System.Process (ProcessHandle, runProcess, terminateProcess, waitForProcess)
import Ruab.Backend.GDB.Representation


-- sync {{{1
data GDBSync = GDBSync {
    gdbHandle    :: ProcessHandle
  , gdbCommand   :: Handle
  , gdbResponse  :: Handle
  , gdbToken     :: IORef Token
  }

sync_start :: Maybe FilePath -> IO (Either String GDBSync)
sync_start workdir = do
  (commandR,  commandW)  <- createPipe >>= asHandles
  (responseR, responseW) <- createPipe >>= asHandles
  phandle <- runProcess "gdb" ["--interpreter", "mi"]
                 workdir Nothing
                 (Just commandR)
                 (Just responseW)
                 Nothing
  mapM_ (`hSetBuffering` LineBuffering) [commandW, responseR]
  return $ GDBSync phandle commandW responseR
  `catch` (\e -> (return . Left) (show (e :: IOException))) 
  where
  asHandles (f1, f2) = do
    h1 <- fdToHandle f1; h2 <- fdToHandle f2; return (h1, h2)

-- async {{{1
data GDBAsync = GDBAsync {
    gdbSync :: GDBSync
  , gdbCallback  :: Callback
  , gdbPid       :: Int -- TODO
}

type Callback = Output -> IO () -- {{{1

send_command :: GDBAsync -> Command -> IO Token
send_command gdb command = do
  token <- nextToken (gdbSync gdb)
  writeCommand gdb (add_token token command)
  return token

handleOutput :: GDB -> IO ()
handleOutput gdb = readOutput gdb >>= gdbCallback gdb >> handleOutput

async_start :: Maybe FilePath -> Callback -> IO (Either String GDBAsync)
async_start workdir callback = do
  gdbSync' <- sync_start workdir
  tokenRef <- newIORef 0
  pid <- forkIO handleOutput gdb
-- send async command TODO
  return $ GDBAsync gdbSync' callback pid tokenRef
  `catch` (\e -> (return . Left) (show (e :: IOException))) 
  where
  asHandles (f1, f2) = do
    h1 <- fdToHandle f1; h2 <- fdToHandle f2; return (h1, h2)

-- utils {{{1
nextToken :: GDBSync -> IO Token -- {{{2
nextToken gdb = do
  token <- readIORef  (gdbToken gdb)
  writeIORef (gdbToken gdb) (token + 1)
  return token

quit :: GDB -> IO ()
quit gdb = do
  writeCommand gdb (MICommand Nothing "gdb-exit" [] [])
  _ <- waitForProcess (gdbHandle gdb)
  stopThread (gdbPid gdb) 
  return ()

-- synchronous
interact :: GDB -> Command -> IO Output
interact gdb cmd = writeCommand gdb cmd >> readOutput gdb

poll :: GDB -> IO (Maybe Output)
poll gdb = do
  ready <- hReady (gdbResponse gdb)
  if ready
    then fmap Just (readOutput gdb)
    else return Nothing

writeCommand :: GDB -> Command -> IO ()
writeCommand gdb cmd = 
  let cmdstr = render_command cmd in
  do
    hPutStr stdout cmdstr
    hPutStr (gdbCommand gdb) cmdstr

readOutput :: GDB -> IO Output
readOutput gdb = do
  _ <- hWaitForInput (gdbResponse gdb) (-1)
  str <- outputString (gdbResponse gdb)
  hPutStr stdout str
  return (parse_output str)
  where
    outputString handle = outputLines >>= return . unlines
    outputLines = do
      line <- hGetLine handle
      if line == "(gdb) "
        then return [line]
        else outputLines >>= return . (line:)
