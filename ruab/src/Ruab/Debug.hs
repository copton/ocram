module Ruab.Debug
(
  load_debug_info
) where

import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Data.Digest.OpenSSL.MD5 (md5sum)
import Ocram.Ruab
import Prelude hiding (catch)
import Control.Exception (catch, IOException)
import Ruab.Options (Options(optDebugFile))
import System.IO (openFile, IOMode(ReadMode), hClose)
import System.Exit (ExitCode(ExitFailure))

import qualified Data.ByteString.Char8 as BS

load_debug_info :: Options -> IO (Either (ExitCode, String) DebugInfo)
load_debug_info opt = do
  hDi <- openFile (optDebugFile opt) ReadMode
  contents <- BS.hGetContents hDi
  hClose hDi
  case decode_debug_info contents of
    Left why -> (return . Left) (ExitFailure 6, why)
    Right di -> do
      let files = [diTcode di, diEcode di]
      code <- mapM (BS.readFile . fileName) files
      case catMaybes (zipWith verify code files) of
        [] -> (return . Right) di
        err -> (return . Left) (ExitFailure 5, intercalate "\n" err)
  `catch` (\e -> (return . Left) (ExitFailure 4, show (e :: IOException)))
  where
    verify contents file
      | md5sum contents == fileChecksum file = Nothing
      | otherwise = Just $ failed $ fileName file
    failed file = "checksum for '" ++ file ++ "' differs"
