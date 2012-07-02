{-# LANGUAGE TemplateHaskell, ViewPatterns #-}
module Ruab.Frontend
-- exports {{{1
(
  frontend_run
) where

-- imports {{{1
import Control.Monad.Fix (mfix)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (intersperse, find)
import Data.Maybe (fromJust)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Events (Event(Key))
import Graphics.UI.Gtk.Glade (xmlNew, xmlGetWidget)
import Paths_Ruab (getDataFileName)
import Prelude hiding (log, lines)
import Ruab.Core
import Ruab.Frontend.Infos (setHighlight, InfoInstance, render_info, setBreakpoint)
import Ruab.Options (Options)
import Ruab.Util (abort, fromJust_s)

import qualified Data.ByteString.Char8 as BS

frontend_run :: Options -> IO () -- {{{1
frontend_run opt = do
  _ <- mfix (\gui -> do
      core <- core_start opt (callback gui)
      gui' <- loadGui core
      setupGui core gui'
      return gui'
    )
  mainGUI

frontendStop :: GUI -> IO ()  -- {{{2
frontendStop gui = do
  core_stop (guiCore gui)
  mainQuit

-- types {{{1
data Component = Component {
    compCode  :: TextView
  , compInfo  :: TextView
  , compLines :: TextView
  , compLabel :: Label
  , compView  :: Viewport
  , compInfos :: IORef [InfoInstance]
  }

data GUI = GUI { -- {{{2
    guiWin    :: Window
  , guiTcomp  :: Component
  , guiPcomp  :: Component
  , guiEcomp  :: Component
  , guiLog    :: TextView
  , guiView   :: TextView
  , guiInput  :: Entry
  , guiStatus :: Statusbar
  , guiCore   :: Core
  }

-- GUI {{{1
loadGui :: Core -> IO GUI -- {{{2
loadGui core = do
  gladefn <- getDataFileName "ruab.glade"
  _ <- initGUI
  Just xml <- xmlNew gladefn
  window <- xmlGetWidget xml castToWindow "window"
  [ct, cp, ce] <- mapM (loadComponent xml) ["t", "p", "e"]
  [log', view] <- mapM (xmlGetWidget xml castToTextView) ["log", "view"]
  input <- xmlGetWidget xml castToEntry "input"
  status <- xmlGetWidget xml castToStatusbar "status"
  return $ GUI window ct cp ce log' view input status core
  where
    loadComponent xml comp = do
      [code, info, lines] <- mapM (xmlGetWidget xml castToTextView) $ map (comp++) ["code", "info", "lines"]
      label <- xmlGetWidget xml castToLabel (comp++"label")
      view <- xmlGetWidget xml castToViewport (comp++"view")
      infos <- newIORef []
      return $ Component code info lines label view infos

setupGui :: Core -> GUI -> IO () -- {{{2
setupGui core gui = do
  -- text views {{{3
  setupComponent (guiTcomp gui)
    (coreTcode core)
    ("(T-code) " ++ coreTfile core)

  setupComponent (guiPcomp gui)
    (corePcode core)
    "(pre-processed)"

  setupComponent (guiEcomp gui)
    (coreEcode core)
    ("(E-code) " ++ (coreEfile core))

  setupText (guiLog gui) (BS.pack "")
  setupText (guiView gui) (BS.pack "")

  -- log {{{3
  addAppendMarker (guiLog gui)

  -- infos {{{3
  setuptBreakpoints

  -- events {{{3
  _ <- onKeyPress (guiInput gui) (handleInput core gui)
  _ <- onDestroy (guiWin gui) (frontendStop gui)

  -- main window {{{3
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

    setuptBreakpoints = do
      infos <- modifyReadIORef ((compInfos . guiPcomp) gui) modify
      setText ((compInfo . guiPcomp) gui) (render_info infos)
      where
        modify infos = foldl (setBreakpoint 0) infos (possible_breakpoints core)


appendToLog :: GUI -> [String] -> IO ()  -- {{{2
appendToLog gui lines = do
  buffer <- textViewGetBuffer (guiLog gui)
  end <- textBufferGetEndIter buffer
  textBufferInsert buffer end $ (concat $ intersperse "\n" lines) ++ "\n"
  mark <- textBufferGetMark buffer "append"
  textViewScrollToMark (guiLog gui) ($fromJust_s mark) 0 Nothing 

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

highlight :: Component -> Int -> IO () -- {{{2
highlight comp row = do
  scrollToRow comp row
  infos <- modifyReadIORef (compInfos comp) (flip setHighlight row)
  setText (compInfo comp) $ render_info infos

modifyReadIORef :: IORef a -> (a -> a) -> IO a -- {{{2
modifyReadIORef ref f = do
  obj <- readIORef ref
  let obj' = f obj
  writeIORef ref obj'
  return obj'

-- user interaction {{{1
handleInput :: Core -> GUI -> Event -> IO Bool -- {{{2
handleInput core gui (Key _ _ _ [] _ _ _ _ "Return" _) = do
  command <- entryGetText (guiInput gui)
  entrySetText (guiInput gui) ""
  appendToLog gui ["$ " ++ command]
  handleCommand core gui (words command)
  return True

handleInput _ _ _ = return False


log :: GUI -> Bool -> [String] -> IO () -- {{{2
log gui f lines = do
  let prefix = if f then "-> " else "!! "
  appendToLog gui (map (prefix++) lines)


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

handleCommand :: Core -> GUI -> [String] -> IO () -- {{{2
-- help {{{3
handleCommand _ gui ["help"] = displayHelp gui True ""
handleCommand _ gui ("help":what:_) = displayHelp gui True what

-- emap {{{3
handleCommand core gui ["emap", row@(parseInt -> Just _)] =
  let row' = (fromJust . parseInt) row in
  case ecode_row core row' of
    Nothing -> log gui False ["no row mapping found"]
    Just row'' -> do
      log gui True [show row'']
      highlight (guiPcomp gui) row'
      highlight (guiEcomp gui) row''
    
-- osapi {{{3
handleCommand core gui ["osapi"] = log gui True ["OS API: " ++ (concat $ intersperse ", " (os_api core))]

-- pmap {{{3
handleCommand core gui ["pmap", row@(parseInt -> Just _)] =
  let row' = (fromJust . parseInt) row in
  case preprocessed_row core row' of
    Nothing -> log gui False ["invalid row number"]
    Just row'' -> do
      log gui True [show row'']
      highlight (guiTcomp gui) row'
      highlight (guiPcomp gui) row''

-- thread {{{3
handleCommand core gui ["thread"] =
  log gui True $ concatMap printThread (all_threads core)

handleCommand core gui ["thread", tid@(parseInt -> Just _)] =
  let (Just tid') = parseInt tid in
  case find (\(Thread tid'' _ _) -> tid'' == tid') (all_threads core) of
    Nothing -> log gui False ["unknown thread id '" ++ tid ++ "'"]
    Just thread -> log gui True (printThread thread)

-- quit {{{3
handleCommand _ gui ["quit"] = frontendStop gui

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

callback :: GUI -> Callback -- {{{2
callback gui (Left x) = postGUIAsync $ log gui True [show x]
callback gui (Right x) = postGUIAsync $ log gui True [show x]
