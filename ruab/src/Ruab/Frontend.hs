{-# LANGUAGE TemplateHaskell, ViewPatterns #-}
module Ruab.Frontend
-- exports {{{1
(
  ruab_ui
) where

-- imports {{{1
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (fromJust)
import Data.List (intersperse)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade (xmlNew, xmlGetWidget)
import Graphics.UI.Gtk.Gdk.Events (Event(Key))
import Ocram.Ruab (DebugInfo(diPcode, diTcode, diEcode), File(fileName))
import Paths_Ruab (getDataFileName)
import Prelude hiding (log, lines)
import Ruab.Frontend.Internal
import Ruab.Mapping (Context(..), map_preprocessed_row)
import Ruab.Options (Options)
import Ruab.Util (abort, fromJust_s)

import qualified Data.ByteString.Char8 as BS

import Debug.Trace (trace)

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
  input <- xmlGetWidget xml castToEntry "input"
  status <- xmlGetWidget xml castToStatusbar "status"
  state <- newIORef emptyState
  return $ GUI window tcode pcode ecode tinfo pinfo einfo tlines plines elines tlabel plabel elabel log' view input status state

handleInput :: Context -> GUI -> Event -> IO Bool -- {{{2
handleInput ctx gui (Key _ _ _ [] _ _ _ _ "Return" _) = do
  command <- entryGetText (guiInput gui)
  entrySetText (guiInput gui) ""
  log gui ["$ " ++ command]
  handleCommand ctx gui (words command)
  return True

handleInput _ _ _ = return False

log :: GUI -> [String] -> IO () -- {{{2
log gui lines = do
  buffer <- textViewGetBuffer (guiLog gui)
  end <- textBufferGetEndIter buffer
  textBufferInsert buffer end $ (concat $ intersperse "\n" lines) ++ "\n"
  mark <- textBufferGetMark buffer "append"
  textViewScrollToMark (guiLog gui) ($fromJust_s mark) 0 Nothing 

displayHelp :: GUI -> String -> IO () -- {{{2
displayHelp gui "" = log gui $ "available commands:" : commands ++ ["type 'help command' to see more information for a command"]
  where commands = ["pmap"]
displayHelp gui "pmap" = log gui $ ["pmap row", "map a T-code row number to the corresponding row number of the pre-processed T-code"]
displayHelp gui unknown = log gui $ ["unknown command '" ++ unknown ++ "'", "type 'help' to see a list of known commands"]


handleCommand :: Context -> GUI -> [String] -> IO () -- {{{2
-- help {{{3
handleCommand _ gui ["help"] = displayHelp gui ""
handleCommand _ gui ("help":what:_) = displayHelp gui what

-- pmap {{{3
handleCommand ctx gui ["pmap", param@(parseInt -> Just _)] =
  let row = (fromJust . parseInt) param in
  case map_preprocessed_row (ctxPreprocMap ctx) row of
    Nothing -> log gui ["invalid row number"]
    Just row' -> do
      log gui ["-> " ++ show row']
      scrollToRow (guiTcode gui) row
      scrollToRow (guiPcode gui) row'

      state <- readIORef (guiState gui)
      let state' = state {
          stateTinfo = setHighlight row (stateTinfo state)
        , statePinfo = setHighlight row' (statePinfo state)
        }
      updateInfo (guiTinfo gui) (stateTinfo state')
      updateInfo (guiPinfo gui) (statePinfo state')
      writeIORef (guiState gui) state'

-- catch all {{{3
handleCommand _ gui (cmd:_) = displayHelp gui cmd
handleCommand _ _ x = $abort $ "unexpected parameter: " ++ show x

parseInt :: String -> Maybe Int -- {{{3
parseInt txt =
  let readings = reads txt in
  if length readings /= 1 || (snd . head) readings /= ""
    then Nothing
    else Just $ (fst . head) readings

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

scrollToRow :: TextView -> Int -> IO () -- {{{2
scrollToRow tv row = do
    buffer <- textViewGetBuffer tv
    iter <- textBufferGetIterAtLine buffer (row-1)
    mark <- textBufferCreateMark buffer Nothing iter True
    textViewScrollToMark tv mark 0 (Just (0, 0))
    return ()

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
