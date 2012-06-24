{-# LANGUAGE TemplateHaskell, ViewPatterns #-}
module Ruab.Frontend
-- exports {{{1
(
  ruab_ui
) where

-- imports {{{1
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (fromJust)
import Data.List (intersperse, find)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade (xmlNew, xmlGetWidget)
import Graphics.UI.Gtk.Gdk.Events (Event(Key))
import Ocram.Ruab (DebugInfo(..), File(fileName), Thread(..))
import Paths_Ruab (getDataFileName)
import Prelude hiding (log, lines)
import Ruab.Frontend.Internal
import Ruab.Mapping (Context(..), preprocessed_row, ecode_row)
import Ruab.Options (Options)
import Ruab.Util (abort, fromJust_s)

import qualified Data.ByteString.Char8 as BS

ruab_ui :: Options -> Context -> IO () -- {{{1
ruab_ui _ ctx = do
  gui <- loadGui
  setupGui ctx gui
  mainGUI
  return ()

data GUI = GUI { -- {{{2
    guiWin :: Window
  , guiTcode :: TextView
  , guiPcode :: TextView
  , guiEcode :: TextView
  , guiTinfo :: TextView
  , guiPinfo :: TextView
  , guiEinfo :: TextView
  , guiTlines :: TextView
  , guiPlines :: TextView
  , guiElines :: TextView
  , guiTlabel :: Label
  , guiPlabel :: Label
  , guiElabel :: Label
  , guiTview :: Viewport
  , guiPview :: Viewport
  , guiEview :: Viewport
  , guiLog :: TextView
  , guiView :: TextView
  , guiInput :: Entry
  , guiStatus :: Statusbar
  , guiState :: IORef State
  }

data State = State { -- {{{2
    stateTinfo :: [InfoInstance]
  , statePinfo :: [InfoInstance]
  , stateEinfo :: [InfoInstance]
  }

emptyState :: State
emptyState = State [] [] []

updateInfo :: TextView -> [InfoInstance] -> IO () -- {{{2
updateInfo tv infos = do
  buffer <- textViewGetBuffer tv
  textBufferSetText buffer $ render_info infos

loadGui :: IO GUI -- {{{2
loadGui = do
  gladefn <- getDataFileName "ruab.glade"
  _ <- initGUI
  Just xml <- xmlNew gladefn
  window <- xmlGetWidget xml castToWindow "window"
  [tcode, pcode, ecode, tinfo, pinfo, einfo, tlines, plines, elines, log', view] <- mapM (xmlGetWidget xml castToTextView)
    ["tcode", "pcode", "ecode", "tinfo", "pinfo", "einfo", "tlines", "plines", "elines", "log", "view"]
  [tlabel, plabel, elabel] <- mapM (xmlGetWidget xml castToLabel) ["tlabel", "plabel", "elabel"]
  [tview, pview, eview] <- mapM (xmlGetWidget xml castToViewport) ["tview", "pview", "eview"]
  input <- xmlGetWidget xml castToEntry "input"
  status <- xmlGetWidget xml castToStatusbar "status"
  state <- newIORef emptyState
  return $ GUI window tcode pcode ecode tinfo pinfo einfo tlines plines elines tlabel plabel elabel tview pview eview log' view input status state

handleInput :: Context -> GUI -> Event -> IO Bool -- {{{2
handleInput ctx gui (Key _ _ _ [] _ _ _ _ "Return" _) = do
  command <- entryGetText (guiInput gui)
  entrySetText (guiInput gui) ""
  appendToLog gui ["$ " ++ command]
  handleCommand ctx gui (words command)
  return True

handleInput _ _ _ = return False


log :: GUI -> Bool -> [String] -> IO () -- {{{2
log gui f lines = do
  let prefix = if f then "-> " else "!! "
  appendToLog gui (map (prefix++) lines)

appendToLog :: GUI -> [String] -> IO ()  -- {{{3
appendToLog gui lines = do
  buffer <- textViewGetBuffer (guiLog gui)
  end <- textBufferGetEndIter buffer
  textBufferInsert buffer end $ (concat $ intersperse "\n" lines) ++ "\n"
  mark <- textBufferGetMark buffer "append"
  textViewScrollToMark (guiLog gui) ($fromJust_s mark) 0 Nothing 

displayHelp :: GUI -> Bool -> String -> IO () -- {{{2
displayHelp gui _ ""       = log gui True        (("available commands: " ++ (concat $ intersperse ", " commands)) : ["type 'help command' to see more information for a command"])
displayHelp gui f "emap"   = log gui (True && f) ["emap row: map a pre-processed T-code row number to the corresponding row number of the E-code"] 
displayHelp gui f "osapi"  = log gui (True && f) ["osapi: list all blocking functions"]
displayHelp gui f "pmap"   = log gui (True && f) ["pmap row: map a T-code row number to the corresponding row number of the pre-processed T-code"]
displayHelp gui f "thread" = log gui (True && f) ["thread [id]: list information of either all threads or the thread with the given id"]
displayHelp gui f "quit"   = log gui (True && f) ["quit: quit the debugger"]
displayHelp gui _ unknown  = log gui False       ["unknown command '" ++ unknown ++ "'", "type 'help' to see a list of known commands"]

commands :: [String]
commands = ["emap", "osapi", "pmap", "threads", "quit"]

handleCommand :: Context -> GUI -> [String] -> IO () -- {{{2
-- help {{{3
handleCommand _ gui ["help"] = displayHelp gui True ""
handleCommand _ gui ("help":what:_) = displayHelp gui True what

-- emap {{{3
handleCommand ctx gui ["emap", row@(parseInt -> Just _)] =
  let row' = (fromJust . parseInt) row in
  case ecode_row ctx row' of
    Nothing -> log gui False ["no row mapping found"]
    Just row'' -> do
      log gui True [show row'']
      scrollToRow (guiPview gui) (guiPcode gui) row'
      scrollToRow (guiEview gui) (guiEcode gui) row''

      state <- readIORef (guiState gui)
      let state' = state {
          statePinfo = setHighlight row' (statePinfo state)
        , stateEinfo = setHighlight row'' (stateEinfo state)
        }
      updateInfo (guiPinfo gui) (statePinfo state')
      updateInfo (guiEinfo gui) (stateEinfo state')
      writeIORef (guiState gui) state'
    
-- osapi {{{3
handleCommand ctx gui ["osapi"] = log gui True ["OS API: " ++ (concat $ intersperse ", " ((diOsApi . ctxDebugInfo) ctx))]

-- pmap {{{3
handleCommand ctx gui ["pmap", row@(parseInt -> Just _)] =
  let row' = (fromJust . parseInt) row in
  case preprocessed_row ctx row' of
    Nothing -> log gui False ["invalid row number"]
    Just row'' -> do
      log gui True [show row'']
      scrollToRow (guiTview gui) (guiTcode gui) row'
      scrollToRow (guiPview gui) (guiPcode gui) row''

      state <- readIORef (guiState gui)
      let state' = state {
          stateTinfo = setHighlight row' (stateTinfo state)
        , statePinfo = setHighlight row'' (statePinfo state)
        }
      updateInfo (guiTinfo gui) (stateTinfo state')
      updateInfo (guiPinfo gui) (statePinfo state')
      writeIORef (guiState gui) state'

-- thread {{{3
handleCommand ctx gui ["thread"] =
  log gui True $ concatMap printThread ((diThreads . ctxDebugInfo) ctx)

handleCommand ctx gui ["thread", tid@(parseInt -> Just _)] =
  let (Just tid') = parseInt tid in
  case find (\(Thread tid'' _ _) -> tid'' == tid') ((diThreads . ctxDebugInfo) ctx) of
    Nothing -> log gui False ["unknown thread id '" ++ tid ++ "'"]
    Just thread -> log gui True (printThread thread)

-- quit {{{3
handleCommand _ _ ["quit"] = mainQuit

-- catch all {{{3
handleCommand _ gui (cmd:_) = displayHelp gui False cmd
handleCommand _ _ x = $abort $ "unexpected parameter: " ++ show x
-- utils {{{4
parseInt :: String -> Maybe Int -- {{{4
parseInt txt =
  let readings = reads txt in
  if length readings /= 1 || (snd . head) readings /= ""
    then Nothing
    else Just $ (fst . head) readings

printThread :: Thread -> [String] -- {{{4
printThread (Thread tid ts tc) = [show tid ++ ": " ++ ts ++ ": " ++ (concat $ intersperse ", " tc)]

setupGui :: Context -> GUI -> IO () -- {{{2
setupGui ctx gui = do
  -- text views {{{3
  displayText (guiTcode gui) (ctxTcode ctx)
  displayText (guiEcode gui) (ctxEcode ctx)
  displayText (guiPcode gui) ((diPcode . ctxDebugInfo) ctx)
  displayText (guiTinfo gui) (BS.pack "")
  displayText (guiPinfo gui) (BS.pack "")
  displayText (guiEinfo gui) (BS.pack "")
  displayText (guiLog gui) (BS.pack "")
  displayText (guiView gui) (BS.pack "")

  -- log {{{3
  addAppendMarker (guiLog gui)

  -- line numbering {{{3
  displayLines (guiTlines gui) (ctxTcode ctx)
  displayLines (guiPlines gui) ((diPcode . ctxDebugInfo) ctx)
  displayLines (guiElines gui) (ctxEcode ctx)

  -- captions {{{3
  labelSetText (guiTlabel gui) $ "(T-code) " ++ ((fileName . diTcode . ctxDebugInfo) ctx)
  labelSetText (guiPlabel gui) "(pre-processed)"
  labelSetText (guiElabel gui) $ "(E-code) " ++ ((fileName . diEcode . ctxDebugInfo) ctx)

  -- events {{{3
  _ <- onKeyPress (guiInput gui) (handleInput ctx gui)
  _ <- onDestroy (guiWin gui) mainQuit

  -- main window {{{3
  widgetGrabFocus (guiInput gui)
  widgetShowAll (guiWin gui)
  where
    addAppendMarker tv = do -- {{{3
      buffer <- textViewGetBuffer tv
      end <- textBufferGetEndIter buffer
      _ <- textBufferCreateMark buffer (Just "append") end False 
      return ()

scrollToRow :: Viewport -> TextView -> Int -> IO () -- {{{2
scrollToRow vp tv row = do
  buffer <- textViewGetBuffer tv
  maxRow <- textBufferGetLineCount buffer
  adj <- viewportGetVAdjustment vp
  upper <- adjustmentGetUpper adj
  lower <- adjustmentGetLower adj
  pageSize <- adjustmentGetPageSize adj
  let ratio = fromIntegral (row - 1) / fromIntegral (maxRow - 1)
  let range = (upper - pageSize) - lower
  adjustmentSetValue adj $ ratio * range + lower
  viewportSetVAdjustment vp adj

displayText :: TextView -> BS.ByteString -> IO () -- {{{2
displayText tv txt = do
  buffer <- textBufferNew Nothing
  textBufferInsertByteStringAtCursor buffer txt
  textViewSetBuffer tv buffer

displayLines :: TextView -> BS.ByteString -> IO ()  -- {{{2
displayLines tv txt =
  let
    rows = BS.lines txt
    text = concat $ intersperse "\n" $ map show [1..(length rows)]
  in do
    buffer <- textBufferNew Nothing
    textBufferInsertAtCursor buffer text
    textViewSetBuffer tv buffer
