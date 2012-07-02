module Ruab.Core
-- exports {{{1
(
-- context
    core_start, core_stop, Core, Callback
  , coreTcode, corePcode, coreEcode
  , coreTfile, coreEfile
-- threads
  , Thread(Thread)
  , all_threads
-- breakpoints
  , possible_breakpoints
-- OS
  , os_api
-- preprocessor
  , PreprocMap
  , preprocessed_row
  , ecode_row
) where

-- imports {{{1
import Data.Digest.OpenSSL.MD5 (md5sum)
import Data.List (intercalate, find)
import Data.Maybe (catMaybes)
import Ocram.Ruab
import Prelude hiding (catch)
import Ruab.Backend (Backend, Callback, backend_start, backend_stop)
import Ruab.Options (Options(optDebugFile, optBinary))
import System.IO (openFile, IOMode(ReadMode), hClose)

import qualified Data.ByteString.Char8 as BS

data Core = Core { -- {{{1
    crDebugInfo :: DebugInfo
  , crTcode     :: BS.ByteString
  , crEcode     :: BS.ByteString
  , crBackend   :: Backend
  }

core_start :: Options -> Callback -> IO Core -- {{{1
core_start opt callback = do
  backend <- backend_start (optBinary opt) callback
  hDi <- openFile (optDebugFile opt) ReadMode
  contents <- BS.hGetContents hDi
  hClose hDi
  case decode_debug_info contents of
    Left why -> fail why
    Right di -> do
      let files = [diTcode di, diEcode di]
      code@[tcode, ecode] <- mapM (BS.readFile . fileName) files
      case catMaybes (zipWith verify code files) of
        [] -> return (Core di tcode ecode backend)
        err -> fail (intercalate "\n" err)
  where
    verify contents file
      | md5sum contents == fileChecksum file = Nothing
      | otherwise = Just $ failed $ fileName file
    failed file = "checksum for '" ++ file ++ "' differs"

core_stop :: Core -> IO ()
core_stop core = backend_stop (crBackend core)

coreTcode, corePcode, coreEcode :: Core -> BS.ByteString -- {{{1
coreTcode = crTcode
corePcode = diPcode . crDebugInfo
coreEcode = crEcode 

coreTfile, coreEfile :: Core -> String -- {{{1
coreTfile = fileName . diTcode . crDebugInfo
coreEfile = fileName . diEcode . crDebugInfo

possible_breakpoints :: Core -> [Int] -- {{{1
possible_breakpoints core = map (tlocRow . fst) $ diLocMap . crDebugInfo $ core

os_api :: Core -> [String] -- {{{1
os_api = diOsApi . crDebugInfo

all_threads :: Core -> [Thread] -- {{{1
all_threads = diThreads . crDebugInfo

preprocessed_row :: Core -> Int -> Maybe Int -- {{{1
preprocessed_row = map_preprocessed_row . diPpm . crDebugInfo

ecode_row :: Core -> Int -> Maybe Int -- {{{1
ecode_row ctx row = fmap (elocRow . snd) $ find ((row==) . tlocRow . fst) $ (diLocMap . crDebugInfo) ctx
