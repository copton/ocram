module Ruab.Mapping
-- exports {{{1
(
-- context
    load_context
  , Context(..)
-- preprocessor
  , PreprocMap
  , map_preprocessed_row
) where

-- imports {{{1
import Control.Exception (catch, IOException)
import Data.Digest.OpenSSL.MD5 (md5sum)
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Ocram.Ruab
import Prelude hiding (catch)
import Ruab.Options (Options(optDebugFile))
import Ruab.Mapping.Internal (preproc_map, PreprocMap(PreprocMap))
import System.Exit (ExitCode(ExitFailure))
import System.IO (openFile, IOMode(ReadMode), hClose)

import qualified Data.ByteString.Char8 as BS

-- context {{{1
data Context = Context {
    ctxDebugInfo :: DebugInfo
  , ctxTcode :: BS.ByteString
  , ctxEcode :: BS.ByteString
  , ctxPreprocMap :: PreprocMap
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
        [] ->
          let ctx = Context di tcode ecode (preproc_map (diPcode di)) in
          (return . Right) ctx
        err -> (return . Left) (ExitFailure 5, intercalate "\n" err)
  `catch` (\e -> (return . Left) (ExitFailure 4, show (e :: IOException)))
  where
    verify contents file
      | md5sum contents == fileChecksum file = Nothing
      | otherwise = Just $ failed $ fileName file
    failed file = "checksum for '" ++ file ++ "' differs"

-- preprocessor {{{1
map_preprocessed_row :: PreprocMap -> Int -> Maybe Int -- {{{2
map_preprocessed_row (PreprocMap rows locs) row
  | row <= 0 = Nothing
  | otherwise =
    let
      (src, dst) = (last . takeWhile ((<=row) . fst)) locs
      res = dst + (row - src) + 1
    in
      if res > rows
        then Nothing
        else Just res
