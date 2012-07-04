{-# LANGUAGE TemplateHaskell #-}
module Ruab.Core
-- exports {{{1
(
-- context
    core_start, core_stop, core_run, Core, Callback
  , coreTcode, corePcode, coreEcode
  , coreTfile, coreEfile
-- updates
  , StatusUpdate, Status(..)
-- threads
  , Thread(Thread)
  , all_threads
-- breakpoints
  , possible_breakpoints
-- OS
  , os_api
-- preprocessor
  , t2p_row, p2t_row
  , p2e_location, e2p_location
) where

-- imports {{{1
import Control.Monad (when)
import Control.Monad.Fix (mfix)
import Data.Digest.OpenSSL.MD5 (md5sum)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (intercalate, find)
import Data.Maybe (catMaybes)
import Ocram.Ruab (DebugInfo(..), File(..), Thread(..),TLocation(..), decode_debug_info)
import Prelude hiding (catch)
import Ruab.Backend (Backend, Callback, backend_start, backend_stop, set_breakpoint, file_function_location, Breakpoint, backend_run, Notification(..), NotifcationType(..), Event(..), continue_execution, asConst)
import Ruab.Core.Internal
import Ruab.Options (Options(optDebugFile, optBinary))
import Ruab.Util (fromJust_s)
import System.IO (openFile, IOMode(ReadMode), hClose)

import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as BS

data Core = Core { -- {{{1
    crDebugInfo    :: DebugInfo
  , crTcode        :: BS.ByteString
  , crEcode        :: BS.ByteString
  , crBackend      :: Backend
  , crBreakpoints  :: [Breakpoint]
  , crStatusUpdate :: StatusUpdate
  , crLastThread   :: IORef Int
  }

type StatusUpdate = Status -> IO () -- {{{1

data Status 
  = Running Int
  | Break Int

core_start :: Options -> StatusUpdate -> IO Core -- {{{1
core_start opt su = do
  di <- loadDebugInfo
  (tcode, ecode) <- loadFiles di
  lastThread <- newIORef (-1)
  mfix (\core -> do
      (backend, breakpoints) <- setupBackend di (optBinary opt) (coreCallback core)
      return $ Core di tcode ecode backend breakpoints su lastThread
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

core_run :: Core -> IO () -- {{{1
core_run core = backend_run (crBackend core)

core_stop :: Core -> IO () -- {{{1
core_stop core = backend_stop (crBackend core)

-- callback {{{1
coreCallback :: Core -> Callback
coreCallback core (Left (Notification Exec Stopped dict)) =
  let bkptno = read $ $fromJust_s $ asConst =<< Map.lookup "bkptno" dict in
  if bkptno >=1 && bkptno <= length (crBreakpoints core)
    then do
      lastThread <- readIORef (crLastThread core)
      when (bkptno /= lastThread) $ do
        writeIORef (crLastThread core) bkptno
        (crStatusUpdate core) (Running bkptno)
      continue_execution (crBackend core)
    else do
      (crStatusUpdate core) (Break (bkptno - length (crBreakpoints core)))

coreCallback _ x = putStrLn $ "## " ++ show x

-- queries {{{1
coreTcode, corePcode, coreEcode :: Core -> BS.ByteString -- {{{2
coreTcode = crTcode
corePcode = diPcode . crDebugInfo
coreEcode = crEcode 

coreTfile, coreEfile :: Core -> String -- {{{2
coreTfile = fileName . diTcode . crDebugInfo
coreEfile = fileName . diEcode . crDebugInfo

possible_breakpoints :: Core -> [Int] -- {{{2
possible_breakpoints core =
  let
    lm = (diLocMap . crDebugInfo) core
    ppm = (diPpm . crDebugInfo) core
  in
    map ($fromJust_s . t2p_row' ppm . tlocRow . fst) lm

os_api :: Core -> [String] -- {{{2
os_api = diOsApi . crDebugInfo

all_threads :: Core -> [Thread] -- {{{2
all_threads = diThreads . crDebugInfo

t2p_row :: Core -> Int -> Maybe Int -- {{{2
t2p_row core row = t2p_row' ((diPpm . crDebugInfo) core) row

p2t_row :: Core -> Int -> Maybe Int -- {{{2
p2t_row core row = p2t_row' ((diPpm . crDebugInfo) core) row

--t2e_breakpoint :: Core -> Int -> Maybe Int -- {{{2
--t2e_breakpoint core row = fmap (elocRow . snd) $ find ((row==) . tlocRow . fst) $ (diLocMap . crDebugInfo) core

p2e_location = undefined
e2p_location = undefined
