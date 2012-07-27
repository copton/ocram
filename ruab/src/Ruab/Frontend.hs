{-# LANGUAGE TemplateHaskell, ViewPatterns, ScopedTypeVariables #-}
module Ruab.Frontend
-- exports {{{1
(
  run
) where

-- imports {{{1
import Control.Applicative ((<$>))
import Control.Monad.Fix (mfix)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef)
import Data.List (intercalate, find)
import Data.Maybe (fromJust, listToMaybe)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade (xmlNew, xmlGetWidget)
import Paths_Ruab (getDataFileName)
import Prelude hiding (log, lines)
import Reactive.Banana (actuate, compile, reactimate, NetworkDescription, newEvent, accumE)
import Ruab.Frontend.Infos (setHighlight, InfoInstance, render_info, setBreakpoint, infoIsHighlight, setThread)
import Ruab.Frontend.Reactive (event_input)
import Ruab.Options (Options)
import Ruab.Util (fromJust_s, abort)

import qualified Ruab.Core as C
import qualified Data.ByteString.Char8 as BS

-- types {{{1
data Component = Component { -- {{{2
    compCode  :: TextView
  , compInfo  :: TextView
  , compLines :: TextView
  , compLabel :: Label
  , compView  :: Viewport
  }

data GUI = GUI { -- {{{2
    guiWin     :: Window
  , guiTcomp   :: Component
  , guiPcomp   :: Component
  , guiEcomp   :: Component
  , guiLog     :: TextView
  , guiView    :: TextView
  , guiInput   :: Entry
  , guiStatus  :: Statusbar
  , guiState   :: IORef State
  , guiCore    :: C.Context
  }

data State = State { -- {{{2
    stateInfos :: [InfoInstance] -- based on p-code
  }

modifyInfos :: ([InfoInstance] -> [InfoInstance]) -> State -> State
modifyInfos f s = s {stateInfos = f (stateInfos s)}

run :: Options -> IO () -- {{{1
run opt = do
  gui <- mfix (\gui' -> C.setup opt (statusUpdate gui') >>= loadGui)
  setupGui gui
  network <- compile $ createNetwork gui
  actuate network
  mainGUI

frontendStop :: C.Context -> IO ()  -- {{{2
frontendStop core = do
  _ <- C.shutdown core
  mainQuit

-- GUI {{{1
loadGui :: C.Context -> IO GUI -- {{{2
loadGui core = do
  gladefn <- getDataFileName "ruab.glade"
  _ <- initGUI
  Just xml <- xmlNew gladefn
  window <- xmlGetWidget xml castToWindow "window"
  [ct, cp, ce] <- mapM (loadComponent xml) ["t", "p", "e"]
  [log', view] <- mapM (xmlGetWidget xml castToTextView) ["log", "view"]
  input <- xmlGetWidget xml castToEntry "input"
  status <- xmlGetWidget xml castToStatusbar "status"
  state <- newIORef $ State []
  return $ GUI window ct cp ce log' view input status state core
  where
    loadComponent xml comp = do
      [code, info, lines] <- mapM (xmlGetWidget xml castToTextView) $ map (comp++) ["code", "info", "lines"]
      label <- xmlGetWidget xml castToLabel (comp++"label")
      view <- xmlGetWidget xml castToViewport (comp++"view")
      return $ Component code info lines label view

createNetwork :: GUI -> NetworkDescription t () -- {{{2
createNetwork gui = do
  eInput <- event_input (guiInput gui)
  (eLog, fireLog) <- newEvent
  (eInfo, fireInfo) <- newEvent
  let
    eCommand = read <$> eInput
    infos = foldr (flip setBreakpoint 0) [] $ map C.getRow $ C.possible_breakpoints (guiCore gui)
    ePinfo = accumE infos eInfo
    eTinfo = sync C.p2t_row <$> ePinfo
    eEinfo = sync C.p2e_row <$> ePinfo

  -- echoing commands
  reactimate $ log (guiLog gui) . LogEvent LogPrompt . (:[]) <$> eInput
  -- show log output
  reactimate $ log (guiLog gui) <$> eLog
  -- update infos
  reactimate $ update (guiTcomp gui) <$> eTinfo
  reactimate $ update (guiPcomp gui) <$> ePinfo
  reactimate $ update (guiEcomp gui) <$> eEinfo
  -- handle commands
  reactimate $ handleCommand (guiCore gui) fireLog fireInfo <$> eCommand
    where
      sync :: (C.Context -> C.PRow -> Maybe Int) -> [InfoInstance] -> [InfoInstance]
      sync f infos = flip map infos $ \(row, x) ->
        case f (guiCore gui) (C.PRow row) of
          Nothing -> $abort $ "failed to map row: " ++ show row
          Just row' -> (row', x)

      update :: Component -> [InfoInstance] -> IO ()
      update comp infos = do
        setText (compInfo comp) (render_info infos)
        maybe (return ()) (scrollToRow comp . fst) $ find (infoIsHighlight . snd) infos

setupGui :: GUI -> IO () -- {{{2
setupGui gui = do
  setupComponent (guiTcomp gui)
    ((C.t_code . guiCore) gui)
    ("(T-code) " ++ (C.t_file . guiCore) gui)

  setupComponent (guiPcomp gui)
    ((C.p_code . guiCore) gui)
    "(pre-processed)"

  setupComponent (guiEcomp gui)
    ((C.e_code . guiCore) gui)
    ("(E-code) " ++ (C.e_file . guiCore) gui)

  setupText (guiView gui) (BS.pack "")
  setupText (guiLog gui) (BS.pack "")
  addAppendMarker (guiLog gui)
  _ <- onDestroy (guiWin gui) (frontendStop (guiCore gui))
  widgetGrabFocus (guiInput gui)
  widgetShowAll (guiWin gui)

  where
    addAppendMarker tv = do -- {{{3
      buffer <- textViewGetBuffer tv
      end <- textBufferGetEndIter buffer
      _ <- textBufferCreateMark buffer (Just "append") end False 
      return ()

    setupComponent comp code caption = do -- {{{3
      setupText (compCode comp) code
      setupText (compInfo comp) (BS.pack "")
      let rows = BS.lines code
      let range = [1..(length rows)]
      let text = BS.intercalate (BS.singleton '\n') $ map (BS.pack . show) range
      setupText (compLines comp) text
      labelSetText (compLabel comp) caption

    setupText tv txt = do -- {{{3
      buffer <- textBufferNew Nothing
      textBufferInsertByteStringAtCursor buffer txt
      textViewSetBuffer tv buffer

-- UI utils {{{1
scrollToRow :: Component -> Int -> IO () -- {{{2
scrollToRow comp row = do
  buffer <- textViewGetBuffer (compCode comp)
  maxRow <- textBufferGetLineCount buffer
  adj <- viewportGetVAdjustment (compView comp)
  upper <- adjustmentGetUpper adj
  lower <- adjustmentGetLower adj
  pageSize <- adjustmentGetPageSize adj
  let ratio = fromIntegral (row - 1) / fromIntegral (maxRow - 1)
  let range = (upper - pageSize) - lower
  adjustmentSetValue adj $ ratio * range + lower
  viewportSetVAdjustment (compView comp) adj

setText :: TextView -> String -> IO () -- {{{2
setText tv txt = do
  buffer <- textViewGetBuffer tv
  textBufferSetText buffer txt

-- actions {{{1
log :: TextView -> LogEvent -> IO () -- {{{2
log tv (LogEvent lt lines) = do
  let
    prefix = show lt ++ " "
    text = (intercalate "\n" $ map (prefix++) lines) ++ "\n"
  buffer <- textViewGetBuffer tv
  end <- textBufferGetEndIter buffer
  textBufferInsert buffer end text
  mark <- textBufferGetMark buffer "append"
  textViewScrollToMark tv ($fromJust_s mark) 0 Nothing 
  return ()

type FireEvent e = e -> IO ()

handleCommand :: C.Context -> FireEvent LogEvent -> FireEvent InfoEvent -> CommandEvent -> IO () -- {{{2
handleCommand core fireLog fireInfo event = handle event
  where
    handle (CmdEvUnknown cmd) = -- {{{3
      lerror ["unknown command '" ++ cmd ++ "'", "type 'help' for assistance"]

    handle (CmdEvBreakAdd prow) = -- {{{3
      C.add_breakpoint core prow >>= either lerror' (\bp -> do
          loutput ["breakpoint added", show bp]
          fireInfo $ setBreakpoint ((C.getRow . C.breakpointRow) bp) (C.breakpointNumber bp)
        )

    handle CmdEvBreakList = do -- {{{3
      bps <- C.list_breakpoints core
      loutput $ "breakpoints:" : map show bps

    handle CmdEvContinue = -- {{{3
      C.continue core >>= either lerror' (\_ -> loutput' "continued")

    handle CmdEvInterrupt = -- {{{3
      C.interrupt core >>= either lerror' (\_ -> loutput' "interrupted")

    handle CmdEvQuit = -- {{{3
      frontendStop core

    handle (CmdEvHelp Nothing) = -- {{{3
      loutput $ "available commands:" : intercalate ", " (map fst commands) : "type 'help <command>' for more information about <command>" : []

    handle (CmdEvHelp (Just cmd)) = -- {{{3
      loutput (help cmd)

    handle (CmdEvScroll rt srow) = -- {{{3
      let
        f = case rt of     
          Tcode -> fmap C.getRow . C.t2p_row core
          Pcode -> Just
          Ecode -> fmap C.getRow . C.e2p_row core
      in
        case f srow of
          Nothing -> lerror ["invalid row number"]
          Just prow -> fireInfo (setHighlight prow)

    handle CmdEvStart = -- {{{3
      C.run core >>= either lerror' (\_ -> loutput' "interrupted")

    loutput ss = fireLog $ LogEvent LogOutput ss
    loutput' s = loutput [s]
    lerror ss = fireLog $ LogEvent LogError ss
    lerror' s = lerror [s]

-- types {{{1
type InfoEvent = [InfoInstance] -> [InfoInstance]

commands :: [(String, Command)] -- {{{2
commands = [
    ("badd", CmdBreakAdd)
  , ("blist", CmdBreakList)
  , ("continue", CmdContinue)
  , ("help", CmdHelp)
  , ("interrupt", CmdInterrupt)
  , ("quit", CmdQuit)
  , ("scroll", CmdScroll)
  , ("start", CmdStart)
  ]
  
data Command -- {{{2
  = CmdBreakAdd
  | CmdBreakList
  | CmdContinue
  | CmdHelp
  | CmdInterrupt
  | CmdQuit
  | CmdScroll
  | CmdStart

instance Read Command where -- {{{3
  readsPrec _ command = case lookup command commands of
    Nothing -> []
    Just cmd -> [(cmd, "")]

data CommandEvent -- {{{2
  = CmdEvUnknown String
  | CmdEvBreakAdd C.PRow
  | CmdEvBreakList
  | CmdEvContinue
  | CmdEvHelp (Maybe Command)
  | CmdEvInterrupt
  | CmdEvQuit
  | CmdEvScroll RowType Int
  | CmdEvStart

instance Read CommandEvent where -- {{{3
  readsPrec _ cmdev = [(commandEvent cmdev, "")]

data RowType -- {{{3
  = Tcode | Pcode | Ecode

instance Read RowType where -- {{{4
  readsPrec _ rtype = case lookup rtype rowtypes of
    Nothing -> []
    Just rt -> [(rt, "")]
    where
      rowtypes = [
          ("t", Tcode)
        , ("p", Pcode)
        , ("e", Ecode)
        ]

commandEvent :: String -> CommandEvent -- {{{2
commandEvent text =
  let (command:options) = words text in
  case mread command of
    Nothing -> CmdEvUnknown command
    Just cmd -> case cmd of
      CmdBreakAdd -> case options of
        [row@(mread -> Just (_ :: Int))] -> CmdEvBreakAdd $ (C.PRow . fromJust . mread) row
        _ -> CmdEvHelp (Just CmdBreakAdd)

      CmdBreakList -> noopt CmdBreakList CmdEvBreakList options
      
      CmdContinue -> noopt CmdContinue CmdEvContinue options

      CmdHelp -> case options of
        [cmd'] -> case mread cmd' of
          Nothing -> CmdEvUnknown cmd'
          Just cmd'' -> CmdEvHelp (Just cmd'')
        _ -> CmdEvHelp Nothing

      CmdInterrupt -> noopt CmdInterrupt CmdEvInterrupt options

      CmdQuit -> noopt CmdQuit CmdEvQuit options

      CmdScroll -> case options of
        [rtype@((`elem`["t","p","e"]) -> True), row@(mread -> Just (_ :: Int))] ->
          CmdEvScroll (read rtype)  ((fromJust . mread) row)
        _ -> CmdEvHelp (Just CmdScroll)

      CmdStart -> noopt CmdStart CmdEvStart options

  where
    noopt _   ev [] = ev
    noopt cmd _  _  = CmdEvHelp (Just cmd)

    mread x = listToMaybe [y | (y,"") <- reads x] 

help :: Command -> [String] -- {{{2
help CmdInterrupt = ["interrupt: interrupt execution"]
help CmdContinue  = ["continue: continue execution"]
help CmdBreakAdd  = ["badd prow: add a breakpoint"]
help CmdBreakList = ["blist: list all breakpoints"]
help CmdQuit = ["quit: quit ruab"]
help CmdScroll = ["scroll t|p|e row: scroll views to the given row"]
help CmdHelp = ["help: show the list of available commands"]
help CmdStart = ["start: start execution"]

data LogEvent = LogEvent LogType [String] -- {{{2

data LogType -- {{{2
  = LogOutput
  | LogError
  | LogStatus
  | LogPrompt

instance Show LogType where -- {{{2
  show LogOutput = ">"
  show LogError = "!"
  show LogStatus = "@"
  show LogPrompt = "$"

-- old {{{1

statusUpdate :: GUI -> C.StatusUpdate
statusUpdate _ _ = return ()
-- statusUpdate :: GUI -> C.StatusUpdate -- {{{2
-- statusUpdate gui threads = postGUIAsync $ do
--   forM_ threads (\thread -> do
--       when (isJust (C.thProw thread)) $
--         modifyIORef (guiState gui) (modifyInfos (setThread ((C.getRow . fromJust . C.thProw) thread) (C.thId thread)))
--       log gui Status [show thread]
--     )
--   syncComponents gui
