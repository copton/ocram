module Ruab.Frontend
-- exports {{{1
(
  ruab_ui
) where

-- imports {{{1
import Data.List (intersperse)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade (xmlNew, xmlGetWidget)
import Graphics.UI.Gtk.Gdk.Events (Event(Key))
import Ocram.Ruab (DebugInfo(diPcode, diTcode, diEcode), File(fileName))
import Paths_Ruab (getDataFileName)
import Prelude hiding (log)
import Ruab.Debug (Context(..))
import Ruab.Options (Options)

import qualified Data.ByteString.Char8 as BS

import Debug.Trace (trace)

ruab_ui :: Options -> Context -> IO () -- {{{1
ruab_ui opt ctx = do
  gui <- loadGui
  setupGui gui ctx
  mainGUI
  return ()

data GUI = GUI { -- {{{1
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
  }

loadGui :: IO GUI -- {{{2
loadGui = do
  gladefn <- getDataFileName "ruab.glade"
  _ <- initGUI
  Just xml <- xmlNew gladefn
  window <- xmlGetWidget xml castToWindow "window"
  [tcode, pcode, ecode, tinfo, pinfo, einfo, tlines, plines, elines, log, view] <- mapM (xmlGetWidget xml castToTextView)
    ["tcode", "pcode", "ecode", "tinfo", "pinfo", "einfo", "tlines", "plines", "elines", "log", "view"]
  [tlabel, plabel, elabel] <- mapM (xmlGetWidget xml castToLabel) ["tlabel", "plabel", "elabel"]
  input <- xmlGetWidget xml castToEntry "input"
  status <- xmlGetWidget xml castToStatusbar "status"
  return $ GUI window tcode pcode ecode tinfo pinfo einfo tlines plines elines tlabel plabel elabel log view input status

handleInput :: GUI -> Event -> IO Bool -- {{{2
handleInput gui (Key _ _ _ [] _ _ _ _ "Return" _) = do
  command <- entryGetText (guiInput gui)
  entrySetText (guiInput gui) ""
  handleCommand gui command
  return True

handleInput _ _ = return False

log :: GUI -> [String] -> IO () -- {{{2
log gui what = do
  buffer <- textViewGetBuffer (guiLog gui)
  end <- textBufferGetEndIter buffer
  textBufferInsert buffer end $ (concat $ intersperse "\n" what) ++ "\n"

handleCommand :: GUI -> String -> IO () -- {{{2
handleCommand gui what = log gui ["$ " ++ what] >> handle what
  where
    handle "help" = log gui ["help text"]
    handle unknown = log gui ["unknown command '" ++ what ++ "'"]

setupGui :: GUI -> Context -> IO () -- {{{2
setupGui gui ctx = do
  -- text views {{{2
  displayText (guiTcode gui) (ctxTcode ctx)
  displayText (guiEcode gui) (ctxEcode ctx)
  displayText (guiPcode gui) ((diPcode . ctxDebugInfo) ctx)
  displayText (guiTinfo gui) (BS.pack "")
  displayText (guiPinfo gui) (BS.pack "")
  displayText (guiEinfo gui) (BS.pack "")
  displayText (guiLog gui) (BS.pack "")
  displayText (guiView gui) (BS.pack "")

  -- line numbering {{{2
  displayLines (guiTlines gui) (ctxTcode ctx)
  displayLines (guiPlines gui) ((diPcode . ctxDebugInfo) ctx)
  displayLines (guiElines gui) (ctxEcode ctx)

  -- captions {{{2
  labelSetText (guiTlabel gui) ((fileName . diTcode . ctxDebugInfo) ctx)
  labelSetText (guiPlabel gui) "(pre-processed)"
  labelSetText (guiElabel gui) ((fileName . diEcode . ctxDebugInfo) ctx)


  -- events {{{2
  _ <- onKeyPress (guiInput gui) (handleInput gui)
  _ <- onDestroy (guiWin gui) mainQuit

  -- main window {{{2
  widgetShowAll (guiWin gui)

displayText :: TextView -> BS.ByteString -> IO () -- {{{3
displayText tv txt = do
  buffer <- textBufferNew Nothing
  textBufferInsertByteStringAtCursor buffer txt
  textViewSetBuffer tv buffer

displayLines :: TextView -> BS.ByteString -> IO ()  -- {{{3
displayLines tv txt =
  let
    lines = BS.lines txt
    text = concat $ intersperse "\n" $ map show [1..(length lines)]
  in do
    buffer <- textBufferNew Nothing
    textBufferInsertAtCursor buffer text
    textViewSetBuffer tv buffer
