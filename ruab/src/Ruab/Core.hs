{-# LANGUAGE TemplateHaskell #-}
module Ruab.Core
-- exports {{{1
(
-- context
    setup, shutdown, Context
  , t_code, p_code, e_code
  , t_file, e_file
-- commands
  , run, interrupt, continue
  , add_breakpoint
-- types
  , StatusUpdate, ThreadStatus(..)
  , Thread(..)
  , PRow, TRow, ERow
  , UserBreakpoint(..)
-- queries
  , possible_breakpoints, list_breakpoints
  , os_api
  , t2p_row, p2t_row, p2e_row, e2p_row
) where

-- imports {{{1
import Control.Applicative ((<$>), (<*>))
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, putMVar)
import Control.Monad.Fix (mfix)
import Control.Monad (forM)
import Data.Digest.OpenSSL.MD5 (md5sum)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef)
import Data.List (intercalate, find)
import Data.Maybe (catMaybes, isNothing)
import Prelude hiding (catch)
import Ruab.Core.Internal
import Ruab.Options (Options(optDebugFile))
import Ruab.Util (fromJust_s, abort)
import System.IO (openFile, IOMode(ReadMode), hClose)

import qualified Data.ByteString.Char8 as BS
import qualified Data.IntMap as IM
import qualified Ocram.Ruab as R
import qualified Data.Set as S
import qualified Ruab.Backend as B

-- types {{{1
data Context = Context { -- {{{2
    ctxDebugInfo     :: R.DebugInfo
  , ctxTcode         :: BS.ByteString
  , ctxEcode         :: BS.ByteString
  , ctxBackend       :: B.Context
  , ctxStatusUpdate  :: StatusUpdate
  , ctxSync          :: MVar ()
  , ctxState         :: IORef State
  }

data State = State { -- {{{2
    stateExecution       :: Execution
  , stateThreads         :: IM.IntMap Thread
  , stateBreakpoints     :: IM.IntMap Breakpoint
  , stateUserBreakpoints :: Int
  } 

data Execution
  = ExInterrupted
  | ExRunning
  | ExShutdown
  | ExWaiting
  deriving Show

data Breakpoint = Breakpoint { -- {{{2
    bkptNumber :: Int
  , bkptType   :: BreakpointType
  }

data BreakpointType
  = BkptUser UserBreakpoint
  | BkptThreadExecution Int -- Thread ID
  | BkptCriticalCall Int Int -- Thread ID, erow

data BreakCondition
  = BreakCondition
  deriving Show

data UserBreakpoint = UserBreakpoint {
    breakpointNumber :: Int
  , breakpointRow    :: PRow
  , breakpointCondition :: Maybe BreakCondition
  } deriving Show

data Thread = Thread { -- {{{2
    thId      :: Int
  , thStart   :: String
  , thStatus  :: ThreadStatus
  , thProw    :: Maybe PRow
  } deriving Show


data ThreadStatus
  = Waiting     -- not yet started
  | Blocked     -- called a blocking function
  | Running     -- currently executing
  | Stopped Int -- stopped at user breakpoint
  deriving (Show, Eq)

data Event -- {{{2
  = EvRun
  | EvInterrupt
  | EvContinue
  | EvShutdown
  | EvBreak PRow
  | EvStopped Int -- breakpoint number

data Result -- {{{2
  = ResNothing
  | ResBreak UserBreakpoint

type StatusUpdate = [Thread] -> IO () -- {{{2

setup :: Options -> StatusUpdate -> IO Context -- {{{1
setup opt su = do
  di <- loadDebugInfo
  (tcode, ecode) <- loadFiles di
  let threads = IM.fromList $ map (\t -> (R.threadId t, Thread (R.threadId t) (R.threadStart t) Waiting Nothing)) $ R.diThreads di
  stateRef <- newIORef (State ExWaiting threads IM.empty 0)
  sync <- newEmptyMVar
  ctx <- mfix (\ctx' -> do
      let callback = B.Callback (streamCallback ctx') (stoppedCallback ctx') (notifyCallback ctx')
      backend <- B.setup opt callback
      return $ Context di tcode ecode backend su sync stateRef
    )
  breakpoints <- coreBreakpoints ctx
  let bm = IM.fromList $ map (\b -> (bkptNumber b, b)) breakpoints
  modifyIORef stateRef (\state -> (state {stateBreakpoints = bm}))
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

    coreBreakpoints ctx = (++) <$> threadExecutionBreakpoints <*> criticalCallBreakpoints
      where
        backend = ctxBackend ctx
        threads = (R.diThreads . ctxDebugInfo) ctx
        blockingCalls = filter R.locIsBlockingCall $ (R.diLocMap . ctxDebugInfo) ctx
        efile = (R.fileName . R.diEcode . ctxDebugInfo) ctx
        threadExecutionBreakpoints = forM threads (\thread ->
            let
              function = R.threadExecution thread
              location = B.file_function_location efile function
            in do
              breakpoint <- B.set_breakpoint backend location
              return $ Breakpoint (B.bkptNumber breakpoint) (BkptThreadExecution (R.threadId thread))
          )
        criticalCallBreakpoints = forM blockingCalls (\loc ->
            let
              location = B.file_line_location efile ((R.elocRow . R.locEloc) loc)
              tid = ($fromJust_s . R.locThreadId) loc
              erow = (R.elocRow . R.locEloc) loc
            in do
              breakpoint <- B.set_breakpoint backend location
              return $ Breakpoint (B.bkptNumber breakpoint) (BkptCriticalCall erow tid)
          )

-- events {{{1
run :: Context -> IO (Either String ()) -- {{{2
run ctx = onEvent ctx EvRun >>= noresult

interrupt :: Context -> IO (Either String ()) -- {{{2
interrupt ctx = onEvent ctx EvInterrupt >>= noresult

continue :: Context -> IO (Either String ()) -- {{{2
continue ctx = onEvent ctx EvContinue >>= noresult

shutdown :: Context -> IO (Either String ()) -- {{{2
shutdown ctx = onEvent ctx EvShutdown >>= noresult

add_breakpoint :: Context -> PRow -> IO (Either String UserBreakpoint) -- {{{2
add_breakpoint ctx prow = do
  res <- onEvent ctx (EvBreak prow)
  case res of
    Left e -> (return . Left) e
    Right (ResBreak bp) -> (return . Right) bp
    _ -> $abort "unexpected value"

-- callbacks {{{2
streamCallback :: Context -> B.Stream -> IO ()
streamCallback _ stream = putStrLn $ "## " ++ show stream

notifyCallback :: Context -> B.Notification -> IO ()
notifyCallback _ notification = putStrLn $ "## " ++ show notification

stoppedCallback :: Context -> B.Stopped -> IO ()
stoppedCallback ctx stopped =
  case B.stoppedReason stopped of
    B.BreakpointHit _ number ->
      onEvent ctx (EvStopped number) >>= either ($abort . show) (return . const ())
    B.EndSteppingRange -> return () -- TODO

-- utils
noresult :: Either String Result -> IO (Either String ())
noresult (Left s) = (return . Left) s
noresult (Right ResNothing) = (return . Right) ()
noresult _ = $abort "unexpected parameter"

handleEvent :: Context -> State -> Event -> Execution -> Either String (IO (State, Result)) -- {{{1

-- EvRun {{{3
handleEvent ctx state EvRun ExWaiting  = Right $ do
  B.run (ctxBackend ctx)
  let state' = state {stateExecution = ExRunning}
  return (state', ResNothing)

handleEvent _   _     EvRun ExInterrupted = alreadyRunning
handleEvent _   _     EvRun ExRunning     = alreadyRunning
handleEvent _   _     EvRun ExShutdown    = alreadyShutdown

-- EvShutdown {{{3
handleEvent _   _     EvShutdown ExShutdown = alreadyShutdown
handleEvent ctx state EvShutdown _          = Right $ do
  B.shutdown (ctxBackend ctx)
  let state' = state {stateExecution = ExShutdown}
  return (state', ResNothing)

-- EvInterrupt {{{3
handleEvent ctx state EvInterrupt ExRunning     = Right $ do
  B.interrupt (ctxBackend ctx)
  let runningThread = IM.filter (isNothing . thProw) (stateThreads state)
  state' <- case IM.size runningThread of
    0 -> return state
    1 ->
      let 
        efile = (R.fileName . R.diEcode . ctxDebugInfo) ctx
        [thread] = IM.elems runningThread
        update frm t = Just $ t {thProw = (e2p_row ctx . B.frameLine) frm}
      in do
        stack <- B.backtrace (ctxBackend ctx) 
        let frame = $fromJust_s $ find ((efile==) . B.frameFile) (B.stackFrames stack)
        putStrLn ("XXX: " ++ show frame ++ "/" ++ (show . e2p_row ctx . B.frameLine) frame)
        let threads = IM.update (update frame) (thId thread) (stateThreads state)
        return $ state {stateThreads = threads}
  
    _ -> $abort $ "multiple threads without row information: " ++ show runningThread
  (ctxStatusUpdate ctx) ((IM.elems . stateThreads) state')
  let state'' = state' {stateExecution = ExInterrupted}
  return (state'', ResNothing)

handleEvent _   state EvInterrupt ExInterrupted = nop state
handleEvent _   _     EvInterrupt ExWaiting     = notRunning
handleEvent _   _     EvInterrupt ExShutdown    = alreadyShutdown

-- EvContinue {{{3
handleEvent ctx state EvContinue ExInterrupted = Right $ do
  B.continue (ctxBackend ctx)
  let state' = state {stateExecution = ExRunning}
  return (state', ResNothing)

handleEvent _   state EvContinue ExRunning     = nop state
handleEvent _   _     EvContinue ExShutdown    = alreadyShutdown
handleEvent _   _     EvContinue ExWaiting     = notRunning

-- EvBreak {{{3
handleEvent _   _     (EvBreak _   ) ExShutdown  = alreadyShutdown
handleEvent _   _     (EvBreak _   ) ExRunning   = Left "cannot set breakpoint while debugger is running."
handleEvent ctx state (EvBreak prow) _           = case p2e_row ctx prow of
  Nothing -> Left "invalid row number"
  Just erow -> Right $ do
    let bnum = stateUserBreakpoints state + 1
    let efile = (R.fileName . R.diEcode . ctxDebugInfo) ctx
    bkpt <- B.set_breakpoint (ctxBackend ctx) (B.file_line_location efile erow)
    let bid = B.bkptNumber bkpt
    let userBp = UserBreakpoint bnum prow Nothing
    let breakpoint = Breakpoint bid (BkptUser userBp)
    let bkpts = IM.insert bid breakpoint (stateBreakpoints state)
    let state' = state {stateUserBreakpoints = bnum, stateBreakpoints = bkpts}
    return (state', ResBreak userBp)

-- EvStopped {{{3
handleEvent ctx state (EvStopped bid) ExRunning     =
  let
    bkpt = $fromJust_s $ IM.lookup bid (stateBreakpoints state)
  in case bkptType bkpt of
    BkptUser ub ->
      let state' = (state {stateThreads = IM.map go (stateThreads state)}) in
      Right $ return (state', ResNothing)
      where
        go thread
          | thStatus thread == Running = thread {thStatus = Stopped (breakpointNumber ub), thProw = Just (breakpointRow ub)}
          | otherwise = thread

    BkptThreadExecution tid -> Right $ do
      B.continue (ctxBackend ctx)
      let state' = setThread state tid (\t -> t {thStatus = Running, thProw = Nothing})
      return (state', ResNothing)
      
    (BkptCriticalCall tid erow) -> Right $ do
      B.continue (ctxBackend ctx)
      let state' = setThread state tid (\t -> t {thStatus = Blocked, thProw = e2p_row ctx erow})
      return (state', ResNothing)
      
handleEvent _   state (EvStopped _)   ExInterrupted = nop state
handleEvent ctx state (EvStopped _)   ExShutdown    = $abort "illegal state"
handleEvent ctx state (EvStopped _)   ExWaiting     = $abort "illegal state"

-- utils {{{3
setThread :: State -> Int -> (Thread -> Thread) -> State -- {{{4
setThread state tid f = state {stateThreads = IM.update (Just . f) tid (stateThreads state)}

nop :: State -> Either String (IO (State, Result)) -- {{{4
nop state = Right $ return (state, ResNothing)

atomicIO :: Context -> IO a -> IO a
atomicIO ctx io = let mv = ctxSync ctx in do
  putMVar mv ()
  result <- io
  takeMVar mv
  return result

notRunning, alreadyRunning, alreadyShutdown :: Either String a -- {{{4
notRunning = Left "Debugger is not running"
alreadyRunning = Left "Debugger is already running"
alreadyShutdown = Left "Debugger has already been shut down"

-- utils -- {{{3
onEvent :: Context -> Event -> IO (Either String Result)
onEvent ctx event = atomicIO ctx $ do
  state <- readIORef (ctxState ctx)
  case handleEvent ctx state event (stateExecution state) of
    Left e -> (return . Left) e
    Right action -> do
      (state', result)  <- action
      writeIORef (ctxState ctx) state'
      (return . Right) result

-- queries {{{1
t_code, p_code, e_code :: Context -> BS.ByteString -- {{{2
t_code = ctxTcode
p_code = R.diPcode . ctxDebugInfo
e_code = ctxEcode 

t_file, e_file :: Context -> String -- {{{2
t_file = R.fileName . R.diTcode . ctxDebugInfo
e_file = R.fileName . R.diEcode . ctxDebugInfo

possible_breakpoints :: Context -> [PRow] -- {{{2
possible_breakpoints ctx =
  S.toList $ S.fromList $ map ($fromJust_s . t2p_row ctx . R.tlocRow . R.locTloc) $ (R.diLocMap . ctxDebugInfo) ctx

list_breakpoints :: Context -> IO [UserBreakpoint] -- {{{2
list_breakpoints ctx = atomicIO ctx $ do
  state <- readIORef (ctxState ctx)
  return $ catMaybes $ map get $ IM.elems $ stateBreakpoints state
  where
    get (Breakpoint _ (BkptUser ub)) = Just ub
    get _ = Nothing

os_api :: Context -> [String] -- {{{2
os_api = R.diOsApi . ctxDebugInfo

t2p_row :: Context -> TRow -> Maybe PRow -- {{{2
t2p_row ctx row = t2p_row' ((R.diPpm . ctxDebugInfo) ctx) row

p2t_row :: Context -> PRow -> Maybe TRow -- {{{2
p2t_row ctx prow = p2t_row' ((R.diPpm . ctxDebugInfo) ctx) prow

e2p_row :: Context -> ERow -> Maybe PRow -- {{{2
e2p_row ctx erow =
  let
    tfile = (R.fileName . R.diTcode . ctxDebugInfo) ctx
    lm = (R.diLocMap . ctxDebugInfo) ctx
    ppm = (R.diPpm . ctxDebugInfo) ctx
  in do
    trow <- e2t_row' lm tfile erow
    prow <- t2p_row' ppm trow
    return prow

p2e_row :: Context -> PRow -> Maybe ERow -- {{{2
p2e_row ctx prow =
  let
    tfile = (R.fileName . R.diTcode . ctxDebugInfo) ctx
    lm = (R.diLocMap . ctxDebugInfo) ctx
    ppm = (R.diPpm . ctxDebugInfo) ctx
  in do
    trow <- p2t_row' ppm prow
    t2e_row' lm tfile trow
