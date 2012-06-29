module Ruab.Backend.GDB.IO
(
  -- synchronous
    GDBSync
  , sync_start, sync_quit, interact, poll
  -- asynchronous
  , GDBAsync, Callback
  , async_start, async_quit, send_command
  , module Ruab.Backend.GDB.Representation
) where

-- imports {{{1
import Control.Monad.Fix (mfix)
import Control.Concurrent (forkIO, killThread, ThreadId)
import Control.Exception (IOException, catch)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Prelude hiding (catch, interact)
import Ruab.Backend.GDB.Commands (add_token, gdb_exit)
import Ruab.Backend.GDB.Representation
import System.IO (Handle, hSetBuffering, BufferMode(LineBuffering), hReady, hPutStr, hWaitForInput, hGetLine, stdout)
import System.Posix.IO (fdToHandle, createPipe)
import System.Process (ProcessHandle, runProcess, terminateProcess, waitForProcess)

-- sync {{{1
data GDBSync = GDBSync { -- {{{2
    gdbHandle    :: ProcessHandle
  , gdbCommand   :: Handle
  , gdbResponse  :: Handle
  , gdbToken     :: IORef Token
  }

sync_start :: Maybe FilePath -> IO (Either String GDBSync) -- {{{2
sync_start workdir = do
  (commandR,  commandW)  <- createPipe >>= asHandles
  (responseR, responseW) <- createPipe >>= asHandles
  phandle <- runProcess "gdb" ["--interpreter", "mi"]
                 workdir Nothing
                 (Just commandR)
                 (Just responseW)
                 Nothing
  mapM_ (`hSetBuffering` LineBuffering) [commandW, responseR]
  tokenRef <- newIORef 0
  return . Right $ GDBSync phandle commandW responseR tokenRef
  `catch` (\e -> (return . Left) (show (e :: IOException))) 
  where
  asHandles (f1, f2) = do
    h1 <- fdToHandle f1; h2 <- fdToHandle f2; return (h1, h2)

sync_quit :: GDBSync -> IO () -- {{{2
sync_quit gdb = do
  token <- nextToken gdb
  writeCommand gdb gdb_exit token
  _ <- waitForProcess (gdbHandle gdb)
  return ()

interact :: GDBSync -> Command -> IO Output -- {{{2
interact gdb cmd = nextToken gdb >>= writeCommand gdb cmd >> readOutput gdb

poll :: GDBSync -> IO (Maybe Output) -- {{{2
poll gdb = do
  ready <- hReady (gdbResponse gdb)
  if ready
    then fmap Just (readOutput gdb)
    else return Nothing

-- async {{{1
data GDBAsync = GDBAsync { -- {{{2
    gdbSync     :: GDBSync
  , gdbCallback :: Callback
  , gdbThread   :: ThreadId
}

type Callback = Output -> IO () -- {{{2

async_start :: Maybe FilePath -> Callback -> IO (Either String GDBAsync) -- {{{2
async_start workdir callback = do
  startResult <- sync_start workdir
  case startResult of
    Left e -> (return . Left) e
    Right syncGdb ->
      let mkGdb = GDBAsync syncGdb callback in
      mfix (\tid -> forkIO (handleOutput (mkGdb tid))) >>= return . Right . mkGdb
--      tie (\tid -> handleOutput (mkGdb tid)) >>= return . Right . mkGdb
  `catch` (\e -> (return . Left) (show (e :: IOException))) 
  where
  asHandles (f1, f2) = do
    h1 <- fdToHandle f1; h2 <- fdToHandle f2; return (h1, h2)
  tie :: (ThreadId -> IO()) -> IO (ThreadId)
  tie io = mfix (\tid -> forkIO (io tid))


async_quit :: GDBAsync -> IO () -- {{{2
async_quit gdb = do
  sync_quit (gdbSync gdb)
  killThread (gdbThread gdb)

handleOutput :: GDBAsync -> IO ()
handleOutput gdb = readOutput (gdbSync gdb) >>= gdbCallback gdb >> handleOutput gdb

send_command :: GDBAsync -> Command -> IO Token
send_command gdb command = do
  token <- nextToken (gdbSync gdb)
  writeCommand (gdbSync gdb) command token
  return token

-- utils {{{1
nextToken :: GDBSync -> IO Token -- {{{2
nextToken gdb = do
  token <- readIORef  (gdbToken gdb)
  writeIORef (gdbToken gdb) (token + 1)
  return token

writeCommand :: GDBSync -> Command -> Token -> IO () -- {{{2
writeCommand gdb cmd token = 
  let cmdstr = (render_command . add_token token) cmd in
  do
    hPutStr stdout cmdstr
    hPutStr (gdbCommand gdb) cmdstr

readOutput :: GDBSync -> IO Output -- {{{2
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
