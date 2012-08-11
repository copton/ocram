{-# LANGUAGE TemplateHaskell #-}
module Ruab.Core
-- exports {{{1
(
    setup, Context
  , create_network
  , Command(..), Result(..), Response
  , Status(..)
  , Thread(..), ThreadStatus(..), R.ThreadId
  , R.PRow(..), R.TRow(..), R.ERow(..)
  , UserBreakpoint(..)
  , t_code, p_code, e_code
  , t_file, e_file
  , possible_breakpoints
  , os_api
  , t2p_row, p2t_row, p2e_row, e2p_row
  , ERowMatch(..)
) where

-- imports {{{1
import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad (forM, when)
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
import qualified Data.Map as M
import qualified Ocram.Ruab as R
import qualified Data.Set as S
import qualified Ruab.Backend as B

-- types {{{1
type TtoE = M.Map R.TRow ERowMatch
type EtoT = M.Map R.ERow R.TRow

data Context = Context { -- {{{2
    ctxDebugInfo     :: R.DebugInfo
  , ctxTcode         :: BS.ByteString
  , ctxEcode         :: BS.ByteString
  , ctxTtoE          :: TtoE
  , ctxEtoT          :: EtoT
  }

data Command -- {{{2
  = CmdAddBreakpoint R.PRow [R.ThreadId]
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
  , stateThreads         :: M.Map R.ThreadId Thread
  , stateBreakpoints     :: M.Map B.BkptNumber Breakpoint
  , stateUserBreakpoints :: M.Map BreakpointNumber [B.BkptNumber]
  , stateHide            :: Bool
  }

data Execution -- {{{2
  = ExRunning
  | ExShutdown
  | ExStopped
  | ExWaiting
  deriving (Show, Eq)

data Breakpoint = Breakpoint { -- {{{2
    bkptNumber :: B.BkptNumber
  , bkptType   :: BreakpointType
  } --deriving Show

data BreakpointType -- {{{2
  = BkptUser UserBreakpoint
  | BkptThreadExecution R.ThreadId
  | BkptBlockingCall R.ThreadId R.ERow
  --deriving Show

data Thread = Thread { -- {{{2
    thId      :: R.ThreadId
  , thStart   :: String
  , thStatus  :: ThreadStatus
  , thProw    :: Maybe R.PRow
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

type BreakpointNumber = Int
data UserBreakpoint = UserBreakpoint { -- {{{2
    breakpointNumber :: BreakpointNumber
  , breakpointRow    :: R.PRow
  , breakpointCondition :: [R.ThreadId]
  } deriving Show

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

    handle (B.BreakpointHit _ bn) ExRunning =
      let bkpt = $fromJust_s $ M.lookup bn (stateBreakpoints state) in
      case bkptType bkpt of
        BkptUser ub ->
          case runningThreads state of
            [thread] ->
              if (thId thread `elem` (breakpointCondition ub))
                then return $
                  hide False .
                  setExecution ExStopped .
                  updateThread (thId thread) (\t ->
                      t {thStatus = Stopped (breakpointNumber ub), thProw = Just (breakpointRow ub)}
                    )
                else do
                  B.continue backend
                  return id
            x -> $abort $ "illegal state of threads: " ++ show x
        
        BkptThreadExecution tid -> do
          B.continue backend
          return $ hide True . updateThread tid (\thread ->
              thread {thStatus = Running, thProw = Nothing}
            )

        BkptBlockingCall tid erow ->
          let
            prow = $fromJust_s $ do
              bc <- find ((==erow) . R.elocRow . R.bcEloc) ((R.diBcs . ctxDebugInfo) ctx)
              let trow = R.tlocRow . R.bcTloc $ bc
              t2p_row ctx trow
          in do
            B.continue backend
            return $ hide True . updateThread tid (\thread ->
                thread {thStatus = Blocked, thProw = Just $ prow}
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
      return $
        setExecution ExRunning .
        mapThreads (\thread ->
            if isStopped thread
              then thread {thStatus = Running, thProw = Nothing}
              else thread
          )

    handle CmdContinue ExRunning = do
      _ <- respond ResContinue
      return id

    handle CmdContinue ExShutdown = alreadyShutdown

    handle CmdContinue ExWaiting = notRunning

    -- CmdAddBreakpoint {{{3
    handle (CmdAddBreakpoint _ _) ExShutdown = alreadyShutdown

    handle (CmdAddBreakpoint _ _) ExRunning = failed "cannot set a breakpoint while the debugger is running"

    handle (CmdAddBreakpoint prow selectedThreads) _ = 
      let
        allThreads = map R.threadId $ (R.diThreads . ctxDebugInfo) ctx
        
        addBreakpoints erows availableThreads errTxt =
          let
            tids = case selectedThreads of
              [] -> availableThreads
              x  -> x
          in case filter (not . (`elem` availableThreads)) tids of
            [] ->
              do
                let
                  efile = (R.fileName . R.diEcode . ctxDebugInfo) ctx
                  ubn = M.size (stateUserBreakpoints state) + 1
                  ub  = UserBreakpoint ubn prow tids
                bkpts <- mapM (B.set_breakpoint backend . B.file_line_location efile . R.getERow) erows
                respond (ResAddBreakpoint ub)
                let
                  sbns = map B.bkptNumber bkpts
                  sbs  = map (flip Breakpoint (BkptUser ub)) sbns
                  bm   = M.fromList $ zip sbns sbs
                return $ \state' -> state' {
                    stateBreakpoints = (stateBreakpoints state') `M.union` bm
                  , stateUserBreakpoints = M.insert ubn sbns (stateUserBreakpoints state')
                  }
            xs -> failed $ errTxt ++ ": " ++ show xs

      in case (p2e_row ctx prow) of
        Nothing                 -> failed "invalid row number"
        Just (NonCritical erow) -> addBreakpoints [erow]       allThreads   "invalid thread ids"
        Just (Critical ts)      -> addBreakpoints (map snd ts) (map fst ts) "unreachable breakpoint"
            
    -- CmdInterrupt {{{3
    handle CmdInterrupt ExRunning = do
      B.interrupt backend
      case runningThreads state of
        [] -> do
          respond ResInterrupt 
          return $ setExecution ExStopped

        [thread] -> do
          stack <- B.backtrace backend
          respond ResInterrupt
          let 
            efile = (R.fileName . R.diEcode . ctxDebugInfo) ctx
            frame = $fromJust_s $ find ((efile==) . B.frameFile) (B.stackFrames stack)
          return $ setExecution ExStopped . updateThread (thId thread) (\t ->
              t {thProw = Just $ $fromJust_s $ (e2p_row ctx . R.ERow . B.frameLine) frame}
            )

        x -> $abort $ "illegal state of threads: " ++ show x

    handle CmdInterrupt ExStopped = do
      respond ResInterrupt
      return $ id

    handle CmdInterrupt ExWaiting = notRunning
    handle CmdInterrupt ExShutdown = alreadyShutdown

    -- CmdShutdown {{{3
    handle CmdShutdown ExShutdown = alreadyShutdown

    handle CmdShutdown _ = do
      B.shutdown backend
      respond ResShutdown
      return $ setExecution ExShutdown

    -- CmdListBreakpoints {{{3
    handle CmdListBreakpoints ExShutdown = alreadyShutdown

    handle CmdListBreakpoints _ = do
      respond $ ResListBreakpoints (state2breakpoints state)
      return id
    
    --utils {{{3
    respond r = fResponse (command, Right r)
    failed e  = fResponse (command, Left e) >> return id
    alreadyRunning = failed "debugger is already running"
    alreadyShutdown = failed "debugger is already shut down"
    notRunning = failed "debugger has not been started yet"

-- utils {{{2
runningThreads :: State -> [Thread] -- {{{3
runningThreads = M.elems . M.filter ((Running==) . thStatus) . stateThreads

isStopped :: Thread -> Bool -- {{{3
isStopped thread = case thStatus thread of
  Stopped _ -> True
  _ -> False

-- state updates {{{1
updateThread :: Int -> (Thread -> Thread) -> State -> State -- {{{2
updateThread tid f state = state {stateThreads = M.update (Just . f) tid (stateThreads state)}

mapThreads :: (Thread -> Thread) -> State -> State -- {{{2
mapThreads f state = state {stateThreads = M.map f (stateThreads state)}

setExecution :: Execution -> State -> State -- {{{2
setExecution e state = state {stateExecution = e}

hide :: Bool -> State -> State -- {{{2
hide b state = state {stateHide = b}

-- conversion {{{1
state2status :: State -> Status -- {{{2
state2status state = Status (M.elems (stateThreads state)) (stateExecution state)

state2breakpoints :: State -> [UserBreakpoint] -- {{{2
state2breakpoints state = catMaybes $ map extract $ M.elems (stateBreakpoints state)
  where
    extract (Breakpoint _ (BkptUser ub)) = Just ub
    extract _ = Nothing
-- setup and shutdown -- {{{1
setup :: Options -> IO Context -- {{{2
setup opt = do
  di <- loadDebugInfo
  (tcode, ecode) <- loadFiles di
  let
    tfile = (R.fileName . R.diTcode) di
    bps = filter ((tfile==) . R.tlocFile . R.bpTloc) (R.diBps di)
    bcs = R.diBcs di
  print bps
  print bcs
  return $ Context di tcode ecode (t2emap bps bcs) (e2tmap bps bcs)
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

    t2emap bps bcs = foldr go M.empty $ fromBcs ++ fromBps
      where
        go :: (R.TRow, R.ERow, Maybe R.ThreadId) -> TtoE -> TtoE
        go (trow, erow, tid) = M.alter (alter erow tid) trow
        alter erow Nothing    Nothing                = Just $ NonCritical erow
        alter erow Nothing    o@(Just (NonCritical erow'))
          | erow == erow'                            = o
          | otherwise                                = $abort $ "debug information is corrupt: " ++ show erow ++ "/" ++ show erow'
        alter _    Nothing    (Just (Critical _))    = $abort "debug information is corrupt"
        alter erow (Just tid) Nothing                = Just $ Critical [(tid, erow)]
        alter _    (Just tid) (Just (NonCritical _)) = $abort "debug information is corrupt"
        alter erow (Just tid) (Just (Critical xs))   = Just $ Critical ((tid, erow):xs)

        fromBps = map ((,,) <$> R.tlocRow . R.bpTloc <*> R.elocRow . R.bpEloc <*> R.bpThreadId) bps
        fromBcs = map ((,,) <$> R.tlocRow . R.bcTloc <*> R.elocRow . R.bcEloc <*> Just . R.bcThreadId) bcs

    e2tmap bps bcs = M.fromList $ fromBcs ++ fromBps
      where
        fromBps = map ((,) <$> R.elocRow . R.bpEloc <*> R.tlocRow . R.bpTloc) bps
        fromBcs = map ((,) <$> R.elocRow . R.bcEloc <*> R.tlocRow . R.bcTloc) bcs 

setupBreakpoints :: Context -> B.Context -> State -> IO State -- {{{2
setupBreakpoints ctx backend state = do
  teb <- threadExecutionBreakpoints
  ccb <- blockingCallBreakpoints
  let bbm = M.fromList $ map (\b -> (bkptNumber b, b)) (teb ++ ccb)
  return $ state {stateBreakpoints = bbm}
  where
    threads = (R.diThreads . ctxDebugInfo) ctx
    blockingCalls = (R.diBcs . ctxDebugInfo) ctx
    efile = (R.fileName . R.diEcode . ctxDebugInfo) ctx
    threadExecutionBreakpoints = forM threads (\thread ->
        let
          function = R.threadExecution thread
          location = B.file_function_location efile function
        in do
          breakpoint <- B.set_breakpoint backend location
          return $ Breakpoint (B.bkptNumber breakpoint) (BkptThreadExecution (R.threadId thread))
      )
    blockingCallBreakpoints = forM blockingCalls (\bc ->
        let
          erow = (R.elocRow . R.bcEloc) bc
          location = B.file_line_location efile (R.getERow erow)
          tid = R.bcThreadId bc
        in do
          breakpoint <- B.set_breakpoint backend location
          return $ Breakpoint (B.bkptNumber breakpoint) (BkptBlockingCall tid erow)
      )

initialState :: Context -> State -- {{{2
initialState ctx =
  let threads = M.fromList $ map (\t -> (R.threadId t, Thread (R.threadId t) (R.threadStart t) Waiting Nothing)) $ R.diThreads $ ctxDebugInfo ctx in
  State ExWaiting threads M.empty M.empty False

-- queries {{{1
t_code, p_code, e_code :: Context -> BS.ByteString -- {{{2
t_code = ctxTcode
p_code = R.diPcode . ctxDebugInfo
e_code = ctxEcode 

t_file, e_file :: Context -> String -- {{{2
t_file = R.fileName . R.diTcode . ctxDebugInfo
e_file = R.fileName . R.diEcode . ctxDebugInfo

possible_breakpoints :: Context -> [R.PRow] -- {{{2
possible_breakpoints ctx =
  S.toList $ S.fromList $ map ($fromJust_s . t2p_row ctx . R.tlocRow . R.bpTloc) $ (R.diBps . ctxDebugInfo) ctx


os_api :: Context -> [String] -- {{{2
os_api = R.diOsApi . ctxDebugInfo

t2p_row :: Context -> R.TRow -> Maybe R.PRow -- {{{2
t2p_row ctx row = t2p_row' ((R.diPpm . ctxDebugInfo) ctx) row

p2t_row :: Context -> R.PRow -> Maybe R.TRow -- {{{2
p2t_row ctx prow = p2t_row' ((R.diPpm . ctxDebugInfo) ctx) prow

e2p_row :: Context -> R.ERow -> Maybe R.PRow -- {{{2
e2p_row ctx erow = do
  trow <- M.lookup erow (ctxEtoT ctx)
  t2p_row' ((R.diPpm . ctxDebugInfo) ctx) trow

p2e_row :: Context -> R.PRow -> Maybe ERowMatch -- {{{2
p2e_row ctx prow =
      p2t_row' ((R.diPpm . ctxDebugInfo) ctx) prow
  >>= flip M.lookup (ctxTtoE ctx)
