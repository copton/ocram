module Ruab.Backend.GDB.IO
(
    GDB
  , start, quit, kill
  , interact, poll
  , module Ruab.Backend.GDB.Representation
) where

import Control.Exception (IOException, catch)
import Prelude hiding (catch, interact)
import System.Posix.IO (fdToHandle, createPipe)
import System.IO (Handle, hSetBuffering, BufferMode(LineBuffering), hReady, hPutStr, hWaitForInput, hGetLine, stdout)
import System.Process (ProcessHandle, runProcess, terminateProcess, waitForProcess)
import Ruab.Backend.GDB.Representation

import Debug.Trace (trace)

data GDB = GDB {
    gdbHandle   :: ProcessHandle
  , gdbCommand  :: Handle
  , gdbResponse :: Handle
  }

start :: Maybe FilePath -> IO (Either String (GDB, Output))
start workdir = do
  (commandR,  commandW)  <- createPipe >>= asHandles
  (responseR, responseW) <- createPipe >>= asHandles
  phandle <- runProcess "gdb" ["--interpreter", "mi"]
                 workdir Nothing
                 (Just commandR)
                 (Just responseW)
                 Nothing
  mapM_ (`hSetBuffering` LineBuffering) [commandW, responseR]
  let gdb = GDB phandle commandW responseR
  output <- readOutput gdb
  return $ Right (gdb , output)
  `catch` (\e -> (return . Left) (show (e :: IOException))) 
  where
  asHandles (f1, f2) = do
    h1 <- fdToHandle f1; h2 <- fdToHandle f2; return (h1, h2)

quit :: GDB -> IO ()
quit gdb = do
  writeCommand gdb (MICommand Nothing "gdb-exit" [] [])
  _ <- waitForProcess (gdbHandle gdb)
  return ()

kill :: GDB -> IO ()
kill gdb = terminateProcess (gdbHandle gdb)

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
  str <- readOutputString (gdbResponse gdb)
  hPutStr stdout str
  return (parse_output str)
  
readOutputString :: Handle -> IO String
readOutputString handle = go >>= return . unlines
  where
    go = do
      line <- hGetLine handle
      if line == "(gdb) "
        then return [line]
        else go >>= return . (line:)



