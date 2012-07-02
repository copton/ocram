module Ruab.Core
-- exports {{{1
(
-- context
    core_start, core_stop, core_run, Core, Callback
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
import Control.Monad.Fix (mfix)
import Data.Digest.OpenSSL.MD5 (md5sum)
import Data.List (intercalate, find)
import Data.Maybe (catMaybes)
import Ocram.Ruab
import Prelude hiding (catch)
import Ruab.Backend (Backend, Callback, backend_start, backend_stop, set_breakpoint, file_function_location, Breakpoint, backend_run)
import Ruab.Options (Options(optDebugFile, optBinary))
import System.IO (openFile, IOMode(ReadMode), hClose)

import qualified Data.ByteString.Char8 as BS

data Core = Core { -- {{{1
    crDebugInfo   :: DebugInfo
  , crTcode       :: BS.ByteString
  , crEcode       :: BS.ByteString
  , crBackend     :: Backend
  , crBreakpoints :: [Breakpoint]
  , crCallback    :: Callback
  }

core_start :: Options -> Callback -> IO Core -- {{{1
core_start opt callback = do
  di <- loadDebugInfo
  (tcode, ecode) <- loadFiles di
  mfix (\core -> do
      (backend, breakpoints) <- setupBackend di (optBinary opt) callback
      return $ Core di tcode ecode backend breakpoints callback
    )
  where
    loadFiles di = do
      let files = [diTcode di, diEcode di]
      code@[tcode, ecode] <- mapM (BS.readFile . fileName) files
      case catMaybes (zipWith verify code files) of
        [] -> return (tcode, ecode)
        err -> fail (intercalate "\n" err)

    loadDebugInfo = do
      hDi <- openFile (optDebugFile opt) ReadMode
      contents <- BS.hGetContents hDi
      hClose hDi
      failOnError $ decode_debug_info contents

    failOnError :: Either String a -> IO a
    failOnError (Left s) = fail s
    failOnError (Right x) = return x

    verify contents file
      | md5sum contents == fileChecksum file = Nothing
      | otherwise = Just $ failed $ fileName file
    failed file = "checksum for '" ++ file ++ "' differs"

    setupBackend di binary callback' = do
      backend <- backend_start binary callback'
      let efile = (fileName . diEcode) di
      let functions = map threadExecution $ diThreads di
      breakpoints <- mapM (set_breakpoint backend . file_function_location efile) functions
      case sequence breakpoints of
        Left e -> fail e
        Right bs -> return (backend, bs)

coreCallback :: Core -> Callback
coreCallback core = crCallback core

core_run :: Core -> IO ()
core_run core = backend_run (crBackend core)

core_stop :: Core -> IO () -- {{{1
core_stop core = backend_stop (crBackend core)

-- queries {{{1
coreTcode, corePcode, coreEcode :: Core -> BS.ByteString -- {{{2
coreTcode = crTcode
corePcode = diPcode . crDebugInfo
coreEcode = crEcode 

coreTfile, coreEfile :: Core -> String -- {{{2
coreTfile = fileName . diTcode . crDebugInfo
coreEfile = fileName . diEcode . crDebugInfo

possible_breakpoints :: Core -> [Int] -- {{{2
possible_breakpoints core = map (tlocRow . fst) $ diLocMap . crDebugInfo $ core

os_api :: Core -> [String] -- {{{2
os_api = diOsApi . crDebugInfo

all_threads :: Core -> [Thread] -- {{{2
all_threads = diThreads . crDebugInfo

preprocessed_row :: Core -> Int -> Maybe Int -- {{{2
preprocessed_row = map_preprocessed_row . diPpm . crDebugInfo

ecode_row :: Core -> Int -> Maybe Int -- {{{2
ecode_row ctx row = fmap (elocRow . snd) $ find ((row==) . tlocRow . fst) $ (diLocMap . crDebugInfo) ctx
