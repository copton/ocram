module Ruab.Frontend
-- exports {{{1
(
  ruab_ui
) where

-- imports {{{1
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade (xmlNew, xmlGetWidget)
import Ocram.Ruab (DebugInfo(diPcode))
import Paths_Ruab (getDataFileName)
import Prelude hiding (log)
import Ruab.Debug (Context(..))
import Ruab.Options (Options)

import qualified Data.ByteString.Char8 as BS

data GUI = GUI { -- {{{1
    guiWin :: Window
  , guiTcode :: TextView
  , guiPcode :: TextView
  , guiEcode :: TextView
  , guiTinfo :: TextView
  , guiPinfo :: TextView
  , guiEinfo :: TextView
  , guiLog :: TextView
  , guiView :: TextView
  , guiInput :: Entry
  , guiStatus :: Statusbar
  }


loadGui :: IO GUI
loadGui = do
  gladefn <- getDataFileName "ruab.glade"
  _ <- initGUI
  Just xml <- xmlNew gladefn
  window <- xmlGetWidget xml castToWindow "window"
  [tcode, ptcode, ecode, tinfo, ptinfo, einfo, log, view] <- mapM (xmlGetWidget xml castToTextView)
    ["tcode", "ptcode", "ecode", "tinfo", "ptinfo", "einfo", "log", "view"]
  input <- xmlGetWidget xml castToEntry "input"
  status <- xmlGetWidget xml castToStatusbar "status"
  return $ GUI window tcode ptcode ecode tinfo ptinfo einfo log view input status

displayText :: TextView -> BS.ByteString -> IO ()
displayText tv txt = do
  buffer <- textBufferNew Nothing
  textBufferInsertByteStringAtCursor buffer txt
  textViewSetBuffer tv buffer

setupGui :: GUI -> Context -> IO ()
setupGui gui ctx = do
  displayText (guiTcode gui) (ctxTcode ctx)
  displayText (guiEcode gui) (ctxEcode ctx)
  displayText (guiPcode gui) ((diPcode . ctxDebugInfo) ctx)
  displayText (guiTinfo gui) BS.empty
  displayText (guiPinfo gui) BS.empty
  displayText (guiEinfo gui) BS.empty
  displayText (guiLog gui) BS.empty
  displayText (guiView gui) BS.empty
  _ <- onDestroy (guiWin gui) mainQuit
  widgetShowAll (guiWin gui)

ruab_ui :: Options -> Context -> IO ()
ruab_ui opt ctx = do
  gui <- loadGui
  setupGui gui ctx
  mainGUI
  return ()
