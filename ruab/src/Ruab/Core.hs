{-# LANGUAGE TemplateHaskell, TupleSections #-}
module Ruab.Core
-- exports {{{1
(
    setup, Context
  , create_network
  , Command(..), Result(..), Response
  , Status(..), Execution(..)
  , Thread(..), ThreadStatus(..), R.ThreadId
  , R.PRow(..), R.TRow(..), R.ERow(..)
  , UserBreakpoint(..), BreakpointNumber
  , t_code, p_code, e_code
  , t_file, e_file
  , possible_breakpoints
  , os_api
  , t2p_row, p2t_row
  , p2e_row, e2p_row
  , ERowMatch(..)
) where

-- imports {{{1
import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad (forM, when)
import Control.Monad.Fix (mfix)
import Data.Digest.OpenSSL.MD5 (md5sum)
import Data.List (intercalate, find, nub)
import Data.Maybe (catMaybes, fromJust, isJust)
import Prelude hiding (catch)
import Ruab.Actor (new_actor, update, monitor_async)
import Ruab.Core.Internal
import Ruab.Options (Options(optDebugFile))
import Ruab.Util (fromJust_s, abort, head_s)
import System.IO (openFile, IOMode(ReadMode), hClose)

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M
import qualified Ocram.Ruab as R
import qualified Ruab.Backend as B

-- types {{{1

data ERowMatch -- {{{2
  = NonCritical R.ERow
  | Critical (M.Map R.ThreadId R.ERow)
  deriving Show

data Context = Context { -- {{{2
    ctxDebugInfo     :: R.DebugInfo
  , ctxTcode         :: BS.ByteString
  , ctxEcode         :: BS.ByteString
  , ctxTtoE          :: M.Map R.TRow ERowMatch
  , ctxEtoT          :: M.Map R.ERow R.TRow
  }

data Command -- {{{2
  = CmdAddBreakpoint R.PRow [R.ThreadId]
  | CmdContinue
  | CmdEvaluate String
  | CmdFilter [R.ThreadId]
  | CmdRemoveBreakpoint BreakpointNumber
  | CmdInterrupt
  | CmdListBreakpoints
  | CmdRun
  | CmdShutdown
  -- deriving Show

data Result -- {{{2
  = ResAddBreakpoint UserBreakpoint
  | ResContinue
  | ResRemoveBreakpoint
  | ResEvaluate String
  | ResFilter
  | ResInterrupt
  | ResListBreakpoints [UserBreakpoint]
  | ResShutdown
  | ResRun

type Response = (Command, Either String Result) -- {{{2

data State = State { -- {{{2
    stateExecution       :: Execution
  , stateThreads         :: M.Map R.ThreadId Thread
  , stateBreakpoints     :: M.Map B.BkptNumber Breakpoint
  , stateUserBreakpoints :: M.Map BreakpointNumber [B.BkptNumber]
  , stateThreadFilter    :: [R.ThreadId]
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
  | Stopped (Maybe Int) -- stopped, maybe at user breakpoint
  deriving (Eq, Show)

isStopped :: ThreadStatus -> Bool
isStopped (Stopped _) = True
isStopped _ = False

data Status = Status { -- {{{2
    statusThreads :: [Thread]
  , statusExecution :: Execution
  , statusThreadFilter :: [R.ThreadId]
  } deriving (Show, Eq)

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
  monitor_async aCore
  let
    fCore f = update aCore $ \state -> do
      state' <- f state
      let
        status  = state2status state
        status' = state2status state'
      when (not (stateHide state') && status /= status') (fStatus status')
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
              if (&&) <$> (`elem` breakpointCondition ub) <*> (`elem` stateThreadFilter state) $ thId thread
                then return $
                  hide False .
                  setExecution ExStopped .
                  updateThread (thId thread) (\t ->
                      t {thStatus = Stopped (Just (breakpointNumber ub)), thProw = Just (breakpointRow ub)}
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
  (r, f) <- handle command  (stateExecution state)
  when (isJust r) (fResponse (command, Right (fromJust r)))
  return $ (hide False . f) state
  where
    -- CmdRun {{{3
    handle CmdRun ExWaiting = do
      B.run backend
      return (Just ResRun, setExecution ExRunning)

    handle CmdRun ExStopped     = alreadyRunning
    handle CmdRun ExRunning     = alreadyRunning
    handle CmdRun ExShutdown    = alreadyShutdown

    -- CmdContinue {{{3
    handle CmdContinue ExRunning = fail "already running"
    handle CmdContinue ExShutdown = alreadyShutdown
    handle CmdContinue ExWaiting = notRunning

    handle CmdContinue ExStopped = do
      B.continue backend
      return (Just ResContinue,
          setExecution ExRunning
        . mapThreads (\thread ->
            if (isStopped . thStatus) thread
              then thread {thStatus = Running, thProw = Nothing}
              else thread
          )
        )


    -- CmdRemoveBreakpoint {{{3
    handle (CmdRemoveBreakpoint _) ExShutdown = alreadyShutdown
    handle (CmdRemoveBreakpoint _) ExRunning  = failed "cannot remove breakpoint while the debugger is running"

    handle (CmdRemoveBreakpoint bid) _ =
      case M.lookup bid (stateUserBreakpoints state) of
        Nothing -> failed "unknown breakpoint id"
        Just bids -> do
          B.remove_breakpoints backend bids
          return (Just ResRemoveBreakpoint, 
              \state' -> state' {
                  stateBreakpoints = foldr M.delete (stateBreakpoints state) bids
                , stateUserBreakpoints = M.delete bid (stateUserBreakpoints state)
                }
            )

    -- CmdAddBreakpoint {{{3
    handle (CmdAddBreakpoint _ _) ExShutdown = alreadyShutdown
    handle (CmdAddBreakpoint _ _) ExRunning  = failed "cannot set a breakpoint while the debugger is running"

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
                let
                  sbns = map B.bkptNumber bkpts
                  sbs  = map (flip Breakpoint (BkptUser ub)) sbns
                  bm   = M.fromList $ zip sbns sbs
                return (Just (ResAddBreakpoint ub),
                    \state' -> state' {
                        stateBreakpoints = (stateBreakpoints state') `M.union` bm
                      , stateUserBreakpoints = M.insert ubn sbns (stateUserBreakpoints state')
                      }
                  )
            xs -> failed $ errTxt ++ ": " ++ show xs

      in case (p2e_row ctx prow) of
        Nothing                 -> failed "invalid row number"
        Just (NonCritical erow) -> addBreakpoints [erow]      allThreads "invalid thread ids"
        Just (Critical m)       -> addBreakpoints (M.elems m) (M.keys m) "unreachable breakpoint"
            
    -- CmdInterrupt {{{3
    handle CmdInterrupt ExRunning = do
      B.interrupt backend
      case runningThreads state of
        [] -> do
          return (Just ResInterrupt, setExecution ExStopped)

        [thread] -> do
          stack <- B.backtrace backend
          let 
            efile = (R.fileName . R.diEcode . ctxDebugInfo) ctx
            frame = $fromJust_s $ find ((efile==) . B.frameFile) (B.stackFrames stack)
          return (Just ResInterrupt, 
              setExecution ExStopped
            . updateThread (thId thread) (\t ->
                t {thProw = e2p_row ctx . R.ERow . B.frameLine $ frame}
              )
            )

        x -> $abort $ "illegal state of threads: " ++ show x

    handle CmdInterrupt ExStopped =
      return (Just ResInterrupt, id)

    handle CmdInterrupt ExWaiting = notRunning
    handle CmdInterrupt ExShutdown = alreadyShutdown

    -- CmdShutdown {{{3
    handle CmdShutdown ExShutdown = alreadyShutdown

    handle CmdShutdown _ = do
      B.shutdown backend
      return (Just ResShutdown, setExecution ExShutdown)

    -- CmdListBreakpoints {{{3
    handle CmdListBreakpoints ExShutdown = alreadyShutdown

    handle CmdListBreakpoints _ =
      return ((Just . ResListBreakpoints . state2breakpoints) state, id)

    -- CmdFilter {{{3
    handle (CmdFilter _) ExShutdown = alreadyShutdown
    handle (CmdFilter _) ExRunning  = failed "cannot set filter while debugger is running"
    handle (CmdFilter threads) _    =
      let
        threads' = case threads of
          [] -> (M.keys . stateThreads) state
          ts -> ts
      in return (Just ResFilter, \state' -> state' {stateThreadFilter = threads'})

    -- CmdEvaluate {{{3
    handle (CmdEvaluate _) ExShutdown = alreadyShutdown
    handle (CmdEvaluate _) ExRunning = failed "cannot evaluate expressions while the debugger is running"
    handle (CmdEvaluate _) ExWaiting = notRunning
    handle (CmdEvaluate texpr) ExStopped = 
      case (M.elems . M.filter (isStopped . thStatus) . stateThreads) state of
        [thread] -> do
          stack <- B.backtrace backend
          let
            fname = (B.frameFunc . $head_s . B.stackFrames) stack
            vm    = (R.diVm . ctxDebugInfo) ctx
            cf    = (R.diCf . ctxDebugInfo) ctx
          case t2e_expr cf vm (thId thread) fname texpr of
            Left err -> failed err
            Right eexpr -> do
              res <- B.evaluate_expression backend eexpr
              case res of
                Left err -> failed err
                Right value -> return (Just (ResEvaluate value), id)
        x -> $abort $ "illegal state of threads: " ++ show x
    
    --utils {{{3
    failed e  = fResponse (command, Left e) >> return (Nothing, id)
    alreadyRunning = failed "debugger is already running"
    alreadyShutdown = failed "debugger is already shut down"
    notRunning = failed "debugger has not been started yet"

-- utils {{{2
runningThreads :: State -> [Thread] -- {{{3
runningThreads = M.elems . M.filter ((Running==) . thStatus) . stateThreads

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
state2status = Status <$> M.elems .stateThreads <*> stateExecution <*> stateThreadFilter

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
    lm = R.getLocMap . R.diLm $ di
    t2em = t2e tfile lm
    e2tm = e2t tfile lm
  return $ Context di tcode ecode t2em e2tm
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

    t2e tfile = foldr go M.empty . M.toList
      where
        go (key, elocs) m =
          let
            erow   = minimum . map R.elocRow $ elocs
            tid    = R.locKeyThread key
            trow   = (R.tlocRow . R.locKeyTloc) key
            tfile' = (R.tlocFile . R.locKeyTloc) key
          in if tfile == tfile'
            then M.alter (alter erow tid) trow m
            else m

        alter erow Nothing    Nothing                    = Just $ NonCritical erow
        alter erow Nothing    (Just (NonCritical erow')) = Just $ NonCritical $ min erow erow'
        alter erow (Just tid) Nothing                    = Just $ Critical $ M.singleton tid erow
        alter erow (Just tid) (Just (Critical m))        = Just $ Critical $ M.alter (alter' erow) tid m
        alter _    Nothing    (Just (Critical _))        = $abort "debug information is corrupt"
        alter _    (Just _  ) (Just (NonCritical _))     = $abort "debug information is corrupt"

        alter' erow Nothing       = Just erow
        alter' erow (Just erow')  = Just $ min erow erow'

    e2t tfile = M.fromList . foldr go [] . M.toList
      where
        go (key, elocs) xs =
          let
            trow = R.tlocRow . R.locKeyTloc $ key
            tfile' = R.tlocFile . R.locKeyTloc $ key
          in if tfile == tfile'
            then map ((, trow) . R.elocRow) elocs ++ xs
            else xs

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
  State ExWaiting threads M.empty M.empty (M.keys threads) False

-- queries {{{1
t_code, p_code, e_code :: Context -> BS.ByteString -- {{{2
t_code = ctxTcode
p_code = R.diPcode . ctxDebugInfo
e_code = ctxEcode 

t_file, e_file :: Context -> String -- {{{2
t_file = R.fileName . R.diTcode . ctxDebugInfo
e_file = R.fileName . R.diEcode . ctxDebugInfo

possible_breakpoints :: Context -> [R.PRow] -- {{{2
possible_breakpoints ctx = nub . map ($fromJust_s . t2p_row ctx) . M.keys . ctxTtoE $ ctx

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
