{-# LANGUAGE ViewPatterns, TemplateHaskell #-} 

module Ruab.Mapping
-- exports {{{1
(
-- context
    load_context
  , Context(..)
-- preprocessor
  , PreprocMap
  , preproc_map
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
import Ruab.Util (abort)
import System.Exit (ExitCode(ExitFailure))
import System.IO (openFile, IOMode(ReadMode), hClose)
import Text.Regex.Posix ((=~))

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

data PreprocMap = PreprocMap Int [(Int, Int)] deriving Show -- {{{2

preproc_map :: BS.ByteString -> PreprocMap -- {{{2
preproc_map ptcode
  | BS.null ptcode = PreprocMap 0 []
  | BS.last ptcode /= '\n' = $abort "pre-processed file should be terminated by a newline"
  | otherwise = PreprocMap (rows - 1) (reverse ppm)
  where
    (first:rest) = init $ BS.split '\n' ptcode
    (ppm, rows) = foldl go ([], 2) rest
    mainFile = case match first of
      (_, (BS.null -> True), _, _) -> $abort $ "unexpected first row in pre-processed file"
      (_, _, _, (_:file:_)) -> file
      x -> $abort $ "unexpected parameter: " ++ show x

    match :: BS.ByteString -> (BS.ByteString, BS.ByteString, BS.ByteString, [BS.ByteString])
    match txt = txt =~ "^# ([0-9]*) \"([^\"]+)\".*$" -- prefix, match, postfix, groups

    go (ppm', row) line = case match line of
      (_, (BS.null -> True), _, _ ) -> (ppm', row + 1)
      (_, _, _, (row':file:_)) -> if file == mainFile
        then (((read . BS.unpack) row', row) : ppm', row + 1)
        else (ppm', row + 1)
      x -> $abort $ "unexpected parameter:" ++ show x
