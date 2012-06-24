module Ruab.Mapping
-- exports {{{1
(
-- context
    load_context
  , Context(..)
-- preprocessor
  , PreprocMap
  , preprocessed_row
  , ecode_row
) where

-- imports {{{1
import Control.Exception (catch, IOException)
import Data.Digest.OpenSSL.MD5 (md5sum)
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Ocram.Ruab
import Prelude hiding (catch)
import Ruab.Options (Options(optDebugFile))
import Ruab.Mapping.Internal
import System.Exit (ExitCode(ExitFailure))
import System.IO (openFile, IOMode(ReadMode), hClose)

import qualified Data.ByteString.Char8 as BS

data Context = Context { -- {{{1
    ctxDebugInfo :: DebugInfo
  , ctxTcode :: BS.ByteString
  , ctxEcode :: BS.ByteString
  }

load_context :: Options -> IO (Either (ExitCode, String) Context) -- {{{1
load_context opt = do
  hDi <- openFile (optDebugFile opt) ReadMode
  contents <- BS.hGetContents hDi
  hClose hDi
  case decode_debug_info contents of
    Left why -> (return . Left) (ExitFailure 6, why)
    Right di -> do
      let files = [diTcode di, diEcode di]
      code@[tcode, ecode] <- mapM (BS.readFile . fileName) files
      case catMaybes (zipWith verify code files) of
        [] -> (return . Right) (Context di tcode ecode)
        err -> (return . Left) (ExitFailure 5, intercalate "\n" err)
  `catch` (\e -> (return . Left) (ExitFailure 4, show (e :: IOException)))
  where
    verify contents file
      | md5sum contents == fileChecksum file = Nothing
      | otherwise = Just $ failed $ fileName file
    failed file = "checksum for '" ++ file ++ "' differs"

preprocessed_row :: Context -> Int -> Maybe Int -- {{{1
preprocessed_row = map_preprocessed_row . diPpm . ctxDebugInfo

ecode_row :: Context -> Int -> Maybe Int -- {{{1
ecode_row ctx = map_ecode_row ((diLocMap . ctxDebugInfo) ctx)
