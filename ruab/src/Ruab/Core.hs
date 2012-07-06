{-# LANGUAGE TemplateHaskell #-}
module Ruab.Core
-- exports {{{1
(
-- context
    start, stop, run, Context
  , t_code, p_code, e_code
  , t_file, e_file
-- updates
  , StatusUpdate, ThreadStatus(..)
-- threads
  , Thread(..)
-- breakpoints
  , possible_breakpoints
-- OS
  , os_api
-- row mapping
  , t2p_row, p2t_row, p2e_row, e2p_row
) where

-- imports {{{1
import Control.Applicative ((<$>), (<*>))
import Control.Concurrent.STM (TVar, newTVarIO, atomically, readTVar, writeTVar)
import Control.Monad.Fix (mfix)
import Control.Monad (forM)
import Data.Digest.OpenSSL.MD5 (md5sum)
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Prelude hiding (catch)
import Ruab.Core.Internal
import Ruab.Options (Options(optDebugFile, optBinary))
import Ruab.Util (fromJust_s)
import System.IO (openFile, IOMode(ReadMode), hClose)

import qualified Data.ByteString.Char8 as BS
import qualified Data.IntMap as IM
import qualified Data.Map as M
import qualified Ocram.Ruab as R
import qualified Data.Set as S
import qualified Ruab.Backend as B

import Debug.Trace (trace)

-- types {{{1
data Context = Context { -- {{{2
    crDebugInfo     :: R.DebugInfo
  , crTcode         :: BS.ByteString
  , crEcode         :: BS.ByteString
  , crBackend       :: B.Context
  , crBreakpoints   :: TVar (IM.IntMap Breakpoint)
  , crStatusUpdate  :: StatusUpdate
  , crState         :: TVar State
  }

data State = State { -- {{{2
    stateThreads :: IM.IntMap Thread
  }

data Breakpoint = Breakpoint { -- {{{2
    bkptNumber :: Int
  , bkptAction :: IO ()
  }

data Thread = Thread { -- {{{2
    thId     :: Int
  , thStatus :: ThreadStatus
  , thProw    :: Maybe Int
  } deriving Show


data ThreadStatus -- {{{2
  = Waiting     -- not yet started
  | Blocked     -- called a blocking function
  | Running     -- currently executing
  | Stopped Int -- stopped at user breakpoint
  deriving Show

type StatusUpdate = [Thread] -> IO () -- {{{2

start :: Options -> StatusUpdate -> IO Context -- {{{1
start opt su = do
  di <- loadDebugInfo
  (tcode, ecode) <- loadFiles di
  let threads = IM.fromList $ map (\t -> (R.threadId t, Thread (R.threadId t) Waiting Nothing)) $ R.diThreads di
  state <- newTVarIO (State threads)
  ctx <- mfix (\ctx' -> do
      backend <- B.start (optBinary opt) (coreCallback ctx')
      bm <- newTVarIO (IM.empty)
      return $ Context di tcode ecode backend bm su state
    )
  breakpoints <- (++) <$> threadExecutionBkpts ctx <*> blockingCallBkpts ctx
  atomically $ writeTVar (crBreakpoints ctx) $ IM.fromList $ map (\b -> (bkptNumber b, b)) breakpoints
  return ctx
  where
    loadFiles di = do
      let files = [R.diTcode di, R.diEcode di]
      code@[tcode, ecode] <- mapM (BS.readFile . R.fileName) files
      case catMaybes (zipWith verify code files) of
        [] -> return (tcode, ecode)
        err -> fail (intercalate "\n" err)

    loadDebugInfo = do
      hDi <- openFile (optDebugFile opt) ReadMode
      contents <- BS.hGetContents hDi
      hClose hDi
      either fail return $ R.decode_debug_info contents

    failOnError :: Either String a -> IO a
    failOnError (Left s) = error s
    failOnError (Right x) = return x

    verify contents file
      | md5sum contents == R.fileChecksum file = Nothing
      | otherwise = Just $ failed $ R.fileName file
    failed file = "checksum for '" ++ file ++ "' differs"

    threadExecutionBkpts ctx =
      let
        backend = crBackend ctx
        threads = (R.diThreads . crDebugInfo) ctx
        efile = (R.fileName . R.diEcode . crDebugInfo) ctx
      in
        forM threads (\thread ->
            let
              function = R.threadExecution thread
              location = B.file_function_location efile function
              action = bkptThreadExecution ctx (R.threadId thread)
            in do
              breakpoint <- either fail return =<< B.set_breakpoint backend location
              return $ Breakpoint (B.bkptNumber breakpoint) action
          )

    blockingCallBkpts ctx =
      let
        backend = crBackend ctx
        blockingCalls = filter R.locIsBlockingCall $ (R.diLocMap . crDebugInfo) ctx
        efile = (R.fileName . R.diEcode . crDebugInfo) ctx
      in
        forM blockingCalls (\loc ->
            let
              location = B.file_line_location efile ((R.elocRow . R.locEloc) loc)
              tid = ($fromJust_s . R.locThreadId) loc
              row = (R.elocRow . R.locEloc) loc
              action = bkptBlockingCall ctx tid row
            in do
              breakpoint <- either fail return =<< B.set_breakpoint backend location
              return $ Breakpoint (B.bkptNumber breakpoint) action
        )

run :: Context -> IO () -- {{{1
run ctx = B.run (crBackend ctx)

stop :: Context -> IO () -- {{{1
stop ctx = B.stop (crBackend ctx)

-- callback {{{1
coreCallback :: Context -> B.Callback -- {{{2
coreCallback ctx x@(Left (B.Notification B.Exec B.Stopped dict)) =
  case M.lookup "bkptno" dict of
    Just value -> do
      bm <- atomically $ readTVar (crBreakpoints ctx)
      let bkptno = (read . $fromJust_s . B.asConst) value
      let bkpt = $fromJust_s $ IM.lookup bkptno bm
      bkptAction bkpt
    Nothing -> putStrLn $ "## " ++ show x

coreCallback _ x = putStrLn $ "## " ++ show x

bkptThreadExecution :: Context -> Int -> IO () -- {{{2
bkptThreadExecution ctx tid = do
  threads <- modifyThread ctx tid (\t -> t {thStatus = Running, thProw = Nothing})
  (crStatusUpdate ctx) threads
  B.continue_execution (crBackend ctx)
  
bkptBlockingCall :: Context -> Int -> Int -> IO () -- {{{2
bkptBlockingCall ctx tid row = do
  threads <- modifyThread ctx tid (\t -> t {thStatus = Blocked, thProw = e2p_row ctx row})
  (crStatusUpdate ctx) threads
  B.continue_execution (crBackend ctx)

bkptUserBreakpoint :: Context -> Int -> Int -> Int -> IO () -- {{{2
bkptUserBreakpoint ctx bid tid row = do
  threads <- modifyThread ctx tid (\t -> t {thStatus = Stopped bid, thProw = e2p_row ctx row})
  (crStatusUpdate ctx) threads
  B.continue_execution (crBackend ctx)
  
modifyThread :: Context -> Int -> (Thread -> Thread) -> IO [Thread] -- {{{2
modifyThread ctx tid f = atomically $ do
  state <- readTVar (crState ctx)
  let threads = IM.update (Just . f) tid (stateThreads state)
  writeTVar (crState ctx) (state {stateThreads = threads})
  return $ IM.elems threads

-- queries {{{1
t_code, p_code, e_code :: Context -> BS.ByteString -- {{{2
t_code = crTcode
p_code = R.diPcode . crDebugInfo
e_code = crEcode 

t_file, e_file :: Context -> String -- {{{2
t_file = R.fileName . R.diTcode . crDebugInfo
e_file = R.fileName . R.diEcode . crDebugInfo

possible_breakpoints :: Context -> [Int] -- {{{2
possible_breakpoints ctx =
  S.toList $ S.fromList $ map ($fromJust_s . t2p_row ctx . R.tlocRow . R.locTloc) $ (R.diLocMap . crDebugInfo) ctx

os_api :: Context -> [String] -- {{{2
os_api = R.diOsApi . crDebugInfo

t2p_row, p2t_row, p2e_row, e2p_row :: Context -> Int -> Maybe Int -- {{{2

t2p_row ctx = t2p_row' $ (R.diPpm . crDebugInfo) ctx

p2t_row ctx = p2t_row' $ (R.diPpm . crDebugInfo) ctx

p2e_row ctx = p2e_row' ((R.diLocMap . crDebugInfo) ctx) ((R.fileName . R.diTcode . crDebugInfo) ctx)

e2p_row ctx = e2p_row' ((R.diLocMap . crDebugInfo) ctx) ((R.fileName . R.diTcode . crDebugInfo) ctx)
