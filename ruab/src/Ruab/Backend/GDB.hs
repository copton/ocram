module Ruab.Backend.GDB
(
    backend_start
  , backend_quit
  , GDB
) where

import Prelude hiding (interact)
import Ruab.Backend.GDB.Representation
import Ruab.Backend.GDB.IO

import Debug.Trace (trace)

backend_start :: FilePath -> IO (Either String GDB)
backend_start binary = do
  res <- start Nothing
  trace "XXX" (return ())
  case res of
    Left e -> (return . Left) e 
    Right (gdb, _) -> do
      _ <- interact gdb $ MICommand Nothing "file-exec-and-symbols" [Option binary Nothing] []
      (return . Right) gdb 

backend_quit :: GDB -> IO ()
backend_quit = quit
