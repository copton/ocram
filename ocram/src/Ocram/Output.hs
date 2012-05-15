module Ocram.Output 
-- export {{{1
(
  dump_ecode, dump_debug_info,
  generate_pal
) where

-- import {{{1
import Control.Exception (handle, IOException)
import Data.List (intersperse)
import Language.C.Pretty (pretty)
import Language.C.Syntax.AST (CTranslUnit)
import Ocram.Analysis (Footprint)
import Ocram.Debug (format_debug_info, LocMap, VarMap)
import Ocram.Options (Options, optOutput, optPalFile, optPalGenerator, optDebugFile)
import Ocram.Text (OcramError, new_error)
import Ocram.Util (fromJust_s)
import System.Exit (ExitCode(..))
import System.IO (openFile, IOMode(WriteMode), hClose, hPutStr, hGetContents)
import System.Process (createProcess, proc, StdStream(CreatePipe), CreateProcess(std_out, std_in, std_err), waitForProcess)

dump_ecode :: Options -> String -> IO (Either [OcramError] ()) -- {{{1
dump_ecode options ecode =
  write (optOutput options) $ ecode ++ "\n"

generate_pal :: Options -> Footprint -> CTranslUnit -> IO (Either [OcramError] ()) -- {{{1
generate_pal options fpr header = case optPalGenerator options of
  Nothing -> (return . Right) ()
  Just generator ->
    let
      target = $fromJust_s (optPalFile options) 
      args = map (concat . intersperse ",") fpr
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

dump_debug_info :: Options -> LocMap -> VarMap -> IO (Either [OcramError] ()) -- {{{1
dump_debug_info opt locMap varMap = case optDebugFile opt of
  Nothing -> (return . Right) ()
  Just file -> write file $ format_debug_info locMap varMap

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

