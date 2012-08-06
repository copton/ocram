{-# LANGUAGE TemplateHaskell #-}
module Ruab.Core
-- exports {{{1
(
    setup, Context
  , create_network
  , Command(..), Result(..), Response
  , Status(..)
  , ThreadStatus(..)
  , Thread(..)
  , PRow, TRow, ERow
  , UserBreakpoint(..)
  , t_code, p_code, e_code
  , t_file, e_file
  , possible_breakpoints
  , os_api
  , t2p_row, p2t_row, p2e_row, e2p_row
) where

-- imports {{{1
import Control.Applicative ((<*>), pure)
import Control.Monad (forM, mplus, when)
import Control.Monad.Fix (mfix)
import Data.Digest.OpenSSL.MD5 (md5sum)
import Data.List (intercalate, find)
import Data.Maybe (catMaybes)
import Prelude hiding (catch)
import Ruab.Actor (new_actor, update)
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
  }

data Command -- {{{2
  = CmdAddBreakpoint PRow
  | CmdContinue
  | CmdInterrupt
  | CmdListBreakpoints
  | CmdShutdown
  | CmdStart
  -- deriving Show

data Result -- {{{2
  = ResAddBreakpoint UserBreakpoint
  | ResContinue
  | ResInterrupt
  | ResListBreakpoints [UserBreakpoint]
  | ResShutdown
  | ResStart

type Response = (Command, Either String Result) -- {{{2

data State = State { -- {{{2
    stateExecution       :: Execution
  , stateThreads         :: IM.IntMap Thread
  , stateBreakpoints     :: IM.IntMap Breakpoint
  , stateUserBreakpoints :: Int
  , stateHide            :: Bool
  }

data Execution -- {{{2
  = ExRunning
  | ExShutdown
  | ExStopped
  | ExWaiting
  deriving (Show, Eq)

data Breakpoint = Breakpoint { -- {{{2
    bkptNumber :: Int
  , bkptType   :: BreakpointType
  } --deriving Show

type ThreadId = Int

data BreakpointType -- {{{2
  = BkptUser UserBreakpoint
  | BkptThreadExecution ThreadId
  | BkptCriticalCall ThreadId ERow
  --deriving Show

data Thread = Thread { -- {{{2
    thId      :: Int
  , thStart   :: String
  , thStatus  :: ThreadStatus
  , thProw    :: Maybe PRow
  } deriving (Show, Eq)

data ThreadStatus -- {{{2
  = Waiting     -- not yet started
  | Blocked     -- called a blocking function
  | Running     -- currently executing
  | Stopped Int -- stopped at user breakpoint
  deriving (Eq, Show)

data Status = Status { -- {{{2
    statusThreads :: [Thread]
  , statusExecution :: Execution
  } deriving Show

data UserBreakpoint = UserBreakpoint { -- {{{2
    breakpointNumber :: Int
  , breakpointRow    :: PRow
  , breakpointCondition :: Maybe BreakCondition
  } deriving Show

data BreakCondition -- {{{2
  = BreakCondition
  deriving Show

-- event network {{{1
type Fire e = e -> IO () -- {{{2

create_network :: Context -> Options -> Fire Response -> Fire Status -> IO (Fire Command) -- {{{2
create_network ctx opt fResponse fStatus = do
  let s0 = initialState ctx
  aCore <- new_actor s0
  let
    fCore f = update aCore $ \state -> do
      state' <- f state
      when (not (stateHide state')) (fStatus $ state2status state')
      return state'

  backend <- mfix (\backend' -> B.setup opt (B.Callback display (fCore . handleStop ctx backend') display))

  fStatus (state2status s0)
  fCore (setupBreakpoints ctx backend)

  return $ fCore . handleCommand ctx backend fResponse
  
display :: Show a => a -> IO () -- {{{2
--display x = putStrLn $ "## " ++ show x
display _ = return ()

type Continuation = (State, IO ()) -> (State, IO ())

handleStop :: Context -> B.Context -> B.Stopped -> State -> IO State -- {{{2
handleStop ctx backend stopped state = handle (B.stoppedReason stopped) (stateExecution state) <*> pure state
  where
    -- EndSteppingRange {{{3
    handle B.EndSteppingRange _ = return id -- TODO

    -- BreakpointHit {{{3
    handle (B.BreakpointHit _ _  ) ExShutdown = $abort "illegal state"
    handle (B.BreakpointHit _ _  ) ExWaiting  = $abort "illegal state"

    handle (B.BreakpointHit _ _  ) ExStopped  = return id

    handle (B.BreakpointHit _ bid) ExRunning =
      let bkpt = $fromJust_s $ IM.lookup bid (stateBreakpoints state) in
      case bkptType bkpt of
        BkptUser ub -> return $ hide False .
            setExecution ExStopped . (mapThreads $ \thread ->
                if thStatus thread == Running
                  then thread {thStatus = Stopped (breakpointNumber ub), thProw = Just (breakpointRow ub)}
                  else thread
              ) 
        
        BkptThreadExecution tid -> do
          B.continue backend
          return $ hide True . updateThread tid (\thread ->
              thread {thStatus = Running, thProw = Nothing}
            )

        BkptCriticalCall tid erow -> do
          B.continue backend
          return $ hide True . updateThread tid (\thread ->
              thread {thStatus = Blocked, thProw = e2p_row ctx erow `mplus` Just (-2)}
            )

handleCommand :: Context -> B.Context -> Fire Response -> Command -> State -> IO State -- {{{2
handleCommand ctx backend fResponse command state = do
  f <- handle command  (stateExecution state)
  return $ (hide False . f) state
  where
    -- CmdStart {{{3
    handle CmdStart ExWaiting = do
      B.run backend
      _ <- respond ResStart
      return $ setExecution ExRunning

    handle CmdStart ExStopped  = alreadyRunning
    handle CmdStart ExRunning  = alreadyRunning
    handle CmdStart ExShutdown = alreadyShutdown

    -- CmdContinue {{{3
    handle CmdContinue ExStopped = do
      B.continue backend
      _ <- respond ResContinue
      return $ setExecution ExRunning

    handle CmdContinue ExRunning = do
      _ <- respond ResContinue
      return id

    handle CmdContinue ExShutdown = alreadyShutdown

    handle CmdContinue ExWaiting = notRunning

    -- CmdAddBreakpoint {{{3
    handle (CmdAddBreakpoint _) ExShutdown = alreadyShutdown

    handle (CmdAddBreakpoint _) ExRunning = failed "cannot set a breakpoint while the debugger is running"

    handle (CmdAddBreakpoint prow) _ = 
      case (p2e_row ctx prow) of
        Nothing -> failed "invalid row number"
        Just erow -> do
          let
            bnum = stateUserBreakpoints state + 1
            efile = (R.fileName . R.diEcode . ctxDebugInfo) ctx
            userBp = UserBreakpoint bnum prow Nothing
          bkpt <- B.set_breakpoint backend (B.file_line_location efile erow)
          let
            bid = B.bkptNumber bkpt
            breakpoint = Breakpoint bid (BkptUser userBp)
            bkpts = IM.insert bid breakpoint (stateBreakpoints state)
          _ <- respond (ResAddBreakpoint userBp)
          return $ \state' -> state' {stateUserBreakpoints = bnum, stateBreakpoints = bkpts}
            
    -- CmdInterrupt {{{3
    handle CmdInterrupt ExRunning = do
      B.interrupt backend
      let runningThread = IM.filter ((==Running). thStatus) (stateThreads state)
      case IM.elems runningThread of
        [] -> do
          _ <- respond ResInterrupt 
          return $ setExecution ExStopped

        [thread] -> do
          stack <- B.backtrace backend
          _ <- respond ResInterrupt
          let 
            efile = (R.fileName . R.diEcode . ctxDebugInfo) ctx
            frame = $fromJust_s $ find ((efile==) . B.frameFile) (B.stackFrames stack)
          return $ setExecution ExStopped . updateThread (thId thread) (\t ->
              t {thProw = (e2p_row ctx . B.frameLine) frame `mplus` Just (-1)}
            )

        x -> $abort $ "illegal state of threads: " ++ show x

    handle CmdInterrupt ExStopped = do
      _ <- respond ResInterrupt
      return $ id

    handle CmdInterrupt ExWaiting = notRunning
    handle CmdInterrupt ExShutdown = alreadyShutdown

    -- CmdShutdown {{{3
    handle CmdShutdown ExShutdown = alreadyShutdown

    handle CmdShutdown _ = do
      B.shutdown backend
      _ <- respond ResShutdown
      return $ setExecution ExShutdown

    -- CmdListBreakpoints {{{3
    handle CmdListBreakpoints ExShutdown = alreadyShutdown

    handle CmdListBreakpoints _ = do
      _ <- respond $ ResListBreakpoints (state2breakpoints state)
      return id
    
    --utils {{{3
    respond r = fResponse (command, Right r) >> return id
    failed e  = fResponse (command, Left e) >> return id
    alreadyRunning = failed "debugger is already running"
    alreadyShutdown = failed "debugger is already shut down"
    notRunning = failed "debugger has not been started yet"

-- state updates {{{1
updateThread :: Int -> (Thread -> Thread) -> State -> State -- {{{2
updateThread tid f state = state {stateThreads = IM.update (Just . f) tid (stateThreads state)}

mapThreads :: (Thread -> Thread) -> State -> State -- {{{2
mapThreads f state = state {stateThreads = IM.map f (stateThreads state)}

setExecution :: Execution -> State -> State -- {{{2
setExecution e state = state {stateExecution = e}

hide :: Bool -> State -> State -- {{{2
hide b state = state {stateHide = b}

-- conversion {{{1
state2status :: State -> Status -- {{{2
state2status state = Status (IM.elems (stateThreads state)) (stateExecution state)

state2breakpoints :: State -> [UserBreakpoint] -- {{{2
state2breakpoints state = catMaybes $ map extract $ IM.elems (stateBreakpoints state)
  where
    extract (Breakpoint _ (BkptUser ub)) = Just ub
    extract _ = Nothing
-- setup and shutdown -- {{{1
setup :: Options -> IO Context -- {{{2
setup opt = do
  di <- loadDebugInfo
  (tcode, ecode) <- loadFiles di
  return $ Context di tcode ecode
  where
    loadFiles di = do
      let files = [R.diTcode di, R.diEcode di]
      code@[tcode, ecode] <- mapM (BS.readFile . R.fileName) files
      case catMaybes (zipWith verify code files) of
        [] -> return (tcode, ecode)
        err -> $abort (intercalate "\n" err)

    loadDebugInfo = do
      hDi <- openFile (optDebugFile opt) ReadMode
      contents <- BS.hGetContents hDi
      hClose hDi
      either fail return $ R.decode_debug_info contents

    verify contents file
      | md5sum contents == R.fileChecksum file = Nothing
      | otherwise = Just $ failed $ R.fileName file
    failed file = "checksum for '" ++ file ++ "' differs"

setupBreakpoints :: Context -> B.Context -> State -> IO State -- {{{2
setupBreakpoints ctx backend state = do
  teb <- threadExecutionBreakpoints
  ccb <- criticalCallBreakpoints
  let bbm = IM.fromList $ map (\b -> (bkptNumber b, b)) (teb ++ ccb)
  return $ state {stateBreakpoints = bbm}
  where
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
          return $ Breakpoint (B.bkptNumber breakpoint) (BkptCriticalCall tid erow)
      )

initialState :: Context -> State -- {{{2
initialState ctx =
  let threads = IM.fromList $ map (\t -> (R.threadId t, Thread (R.threadId t) (R.threadStart t) Waiting Nothing)) $ R.diThreads $ ctxDebugInfo ctx in
  State ExWaiting threads IM.empty 0 False

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

-- list_breakpoints :: Context -> IO [UserBreakpoint] -- {{{2
-- list_breakpoints ctx = atomicIO ctx $ do
--   state <- readIORef (ctxState ctx)
--   return $ catMaybes $ map get $ IM.elems $ stateBreakpoints state
--   where
--     get (Breakpoint _ (BkptUser ub)) = Just ub
--     get _ = Nothing

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

