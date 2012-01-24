module Ocram.Output 
-- export {{{1
(
  pretty_print, dump_pal,
  write_debug_symbols
) where

-- import {{{1
import Control.Exception (handle, IOException)
import Data.List (intersperse)
import Language.C (pretty)
import Ocram.Analysis (Footprint)
import Ocram.Options (Options, optOutput, optPalFile, optPalGenerator)
import Ocram.Text (OcramError, new_error)
import Ocram.Types (Ast, DebugSymbols)
import Ocram.Util (fromJust_s)
import System.Exit (ExitCode(..))
import System.IO (openFile, IOMode(WriteMode), hClose, hPutStr, hGetContents)
import System.Process (createProcess, proc, StdStream(CreatePipe), CreateProcess(std_out, std_in, std_err), waitForProcess)

pretty_print :: Options -> Ast -> IO (Either [OcramError] ()) -- {{{1
pretty_print options ast =
  write (optOutput options) $ (show . pretty) ast

dump_pal :: Options -> Footprint -> Ast -> IO (Either [OcramError] ()) -- {{{1
dump_pal options fpr header = case optPalGenerator options of
  Nothing -> (return . Right) ()
  Just generator ->
    let
      target = $fromJust_s (optPalFile options) 
      args = map (concat . intersperse "," . map show) fpr
      input = (show . pretty) header
      process = (proc generator args) {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe}
    in do
      (Just hin, Just hout, Just herr, p) <- createProcess process
      hPutStr hin input
      hClose hin
      ec <- waitForProcess p
      case ec of
        ExitSuccess -> hGetContents hout >>= write target
        ExitFailure code -> do
          es <- hGetContents herr
          (return . Left) [new_error 2 ("calling external generator failed: (" ++ (show code) ++ ")\n:" ++ es) Nothing]

write_debug_symbols :: Options -> DebugSymbols -> IO (Either [OcramError] ()) -- {{{1
write_debug_symbols _ _ = (return . Right) ()

-- utils {{{1
write :: String -> String -> IO (Either [OcramError] ())
write filename contents = handle exception $ do
    outh <- openFile filename WriteMode
    hPutStr outh contents
    hClose outh
    (return . Right) ()
    where
    exception :: IOException -> IO (Either [OcramError] ())
    exception e = (return . Left) [new_error 1 ("failed to write to '" ++ filename ++ "':\n" ++ show e) Nothing]

