{-# LANGUAGE TemplateHaskell, ViewPatterns #-}
module Ruab.Frontend
-- exports {{{1
(
  run
) where

-- imports {{{1
import Control.Monad.Fix (mfix)
import Control.Monad (when, forM_, zipWithM_)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef)
import Data.List (intersperse, find)
import Data.Maybe (fromJust, catMaybes, isJust)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Events (Event(Key))
import Graphics.UI.Gtk.Glade (xmlNew, xmlGetWidget)
import Paths_Ruab (getDataFileName)
import Prelude hiding (log, lines)
import Ruab.Frontend.Infos (setHighlight, InfoInstance, render_info, setBreakpoint, infoIsHighlight, setThread)
import Ruab.Options (Options)
import Ruab.Util (abort, fromJust_s)

import qualified Ruab.Core as C
import qualified Data.ByteString.Char8 as BS

-- types {{{1
data Component = Component { -- {{{2
    compCode  :: TextView
  , compInfo  :: TextView
  , compLines :: TextView
  , compLabel :: Label
  , compView  :: Viewport
  , compInfos :: IORef [InfoInstance]
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
  , guiCore    :: C.Context
  }

run :: Options -> IO () -- {{{1
run opt = do
  gui <- mfix (\gui' -> C.start opt (statusUpdate gui') >>= loadGui)
  setupGui gui
  mainGUI

frontendStop :: GUI -> IO ()  -- {{{2
frontendStop gui = do
  C.stop (guiCore gui)
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
  return $ GUI window ct cp ce log' view input status core
  where
    loadComponent xml comp = do
      [code, info, lines] <- mapM (xmlGetWidget xml castToTextView) $ map (comp++) ["code", "info", "lines"]
      label <- xmlGetWidget xml castToLabel (comp++"label")
      view <- xmlGetWidget xml castToViewport (comp++"view")
      infos <- newIORef []
      return $ Component code info lines label view infos

setupGui :: GUI -> IO () -- {{{2
setupGui gui = do
  -- text views {{{3
  setupComponent (guiTcomp gui)
    ((C.t_code . guiCore) gui)
    ("(T-code) " ++ (C.t_file . guiCore) gui)

  setupComponent (guiPcomp gui)
    ((C.p_code . guiCore) gui)
    "(pre-processed)"

  setupComponent (guiEcomp gui)
    ((C.e_code . guiCore) gui)
    ("(E-code) " ++ (C.e_file . guiCore) gui)

  setupText (guiLog gui) (BS.pack "")
  setupText (guiView gui) (BS.pack "")

  -- log {{{3
  addAppendMarker (guiLog gui)

  -- infos {{{3
  setuptBreakpoints

  -- events {{{3
  _ <- onKeyPress (guiInput gui) (handleInput gui)
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
      modifyIORef ((compInfos . guiPcomp) gui) modify
      syncView gui
      where
        modify infos = foldl (setBreakpoint 0) infos ((C.possible_breakpoints . guiCore) gui)


append :: TextView -> [String] -> IO ()  -- {{{2
append tv lines = do
  buffer <- textViewGetBuffer tv
  end <- textBufferGetEndIter buffer
  textBufferInsert buffer end $ (concat $ intersperse "\n" lines) ++ "\n"
  mark <- textBufferGetMark buffer "append"
  textViewScrollToMark tv ($fromJust_s mark) 0 Nothing 
  return ()

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

-- user interaction {{{1
handleInput :: GUI -> Event -> IO Bool -- {{{2
handleInput gui (Key _ _ _ [] _ _ _ _ "Return" _) = do
  command <- entryGetText (guiInput gui)
  when (command /= "") $ do
    entrySetText (guiInput gui) ""
    append (guiLog gui) ["$ " ++ command]
    handleCommand gui (words command)
  return True

handleInput _ _ = return False

data Log -- {{{2
  = Output
  | Error
  | Status

instance Show Log where
  show Output = ">"
  show Error = "!"
  show Status = "@"

log :: GUI -> Log -> [String] -> IO () -- {{{2
log gui l lines =
  let prefix = show l ++ " " in
  append (guiLog gui) (map (prefix++) lines)

displayHelp :: GUI -> Log -> String -> IO () -- {{{2
displayHelp gui _ ""       = log gui Output (("available commands: " ++ (concat $ intersperse ", " commands)) : ["type 'help command' to see more information for a command"])
displayHelp gui l "osapi"  = log gui l ["osapi: list all blocking functions"]
displayHelp gui l "tmap"   = log gui l ["tmap row: map a T-code row number to the corresponding row number of the pre-processed T-code"]
displayHelp gui l "pmap"   = log gui l ["pmap row: map a row number from the pre-processed T-code to the corresponding row number of the original T-code"]
displayHelp gui l "quit"   = log gui l ["quit: quit the debugger"]
displayHelp gui l "start"  = log gui l ["start: start debugging the binary"]
displayHelp gui _ unknown  = log gui Error ["unknown command '" ++ unknown ++ "'", "type 'help' to see a list of known commands"]

commands :: [String]
commands = ["tmap", "pmap", "osapi", "quit", "start"]

handleCommand :: GUI -> [String] -> IO () -- {{{2
-- help {{{3
handleCommand gui ["help"] = displayHelp gui Output ""
handleCommand gui ("help":what:_) = displayHelp gui Output what

    
-- osapi {{{3
handleCommand gui ["osapi"] = log gui Output ["OS API: " ++ (concat $ intersperse ", " (C.os_api (guiCore gui)))]

-- pmap {{{3
handleCommand gui ["pmap", row@(parseInt -> Just _)] =
  let prow = (fromJust . parseInt) row in
  case C.p2t_row (guiCore gui) prow of
    Nothing -> log gui Error ["invalid row number"]
    Just trow -> do
      log gui Output [show trow]
      modifyIORef ((compInfos . guiPcomp) gui) (flip setHighlight prow)
      syncView gui

-- tmap {{{3
handleCommand gui ["tmap", row@(parseInt -> Just _)] =
  let trow = (fromJust . parseInt) row in
  case C.t2p_row (guiCore gui) trow of
    Nothing -> log gui Error ["invalid row number"]
    Just prow -> do
      log gui Output [show prow]
      modifyIORef ((compInfos . guiPcomp) gui) (flip setHighlight prow)
      syncView gui


-- quit {{{3
handleCommand gui ["quit"] = frontendStop gui

-- start {{{3
handleCommand gui ["start"] = C.run (guiCore gui)

-- catch all {{{3
handleCommand gui (cmd:_) = displayHelp gui Error cmd
handleCommand _ x = $abort $ "unexpected parameter: " ++ show x
-- utils {{{4
parseInt :: String -> Maybe Int -- {{{4
parseInt txt =
  let readings = reads txt in
  if length readings /= 1 || (snd . head) readings /= ""
    then Nothing
    else Just $ (fst . head) readings

statusUpdate :: GUI -> C.StatusUpdate -- {{{2
statusUpdate gui threads = postGUIAsync $ do
  forM_ threads (\thread ->
      let
        prow = C.thProw thread
        erow = prow >>= C.p2e_row (guiCore gui)
        components = [guiPcomp gui, guiEcomp gui]
        update = \infos row -> setThread ($fromJust_s row) infos (C.thId thread)
      in do
        when (isJust erow) $ do
          infos <- mapM (readIORef . compInfos) components
          let infos' = zipWith update infos [prow, erow]
          zipWithM_ (\comp is -> writeIORef (compInfos comp) is) components infos'
        log gui Status [show thread]
    )
  syncView gui 

syncView :: GUI -> IO () -- {{{2
syncView gui = 
  let
    tcomp = (guiTcomp gui)
    pcomp = (guiPcomp gui)
    ecomp = (guiEcomp gui)
  in do
    pinfos <- readIORef (compInfos pcomp)
    let tinfos = catMaybes $ map syncInfo pinfos
    writeIORef (compInfos tcomp) tinfos
    updateCompView tcomp
    updateCompView pcomp
    updateCompView ecomp
  where
    syncInfo (row, x) = case C.p2t_row (guiCore gui) row of
      Nothing -> Nothing
      Just row' -> Just (row', x)

updateCompView :: Component -> IO () -- {{{2
updateCompView comp = do
  infos <- readIORef (compInfos comp)
  setText (compInfo comp) (render_info infos)
  case find (infoIsHighlight . snd) infos of
    Nothing -> return ()
    Just (row, _) -> scrollToRow comp row

