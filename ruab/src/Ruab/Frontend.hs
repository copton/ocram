module Ruab.Frontend
-- exports {{{1
(
  ruab_ui
) where

-- imports {{{1
import Paths_Ruab (getDataFileName)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade (xmlNew, xmlGetWidget)
import OcramRuab (LocMap, VarMap)
import Prelude hiding (log)

import qualified Data.ByteString.Char8 as BS
import Debug.Trace (trace)

data GUI = GUI { -- {{{1
    guiWin :: Window
  , guiTcode :: TextView
  , guiPtcode :: TextView
  , guiEcode :: TextView
  , guiTinfo :: TextView
  , guiPtinfo :: TextView
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
  trace (gladefn) (return ())
  Just xml <- xmlNew gladefn
  window <- xmlGetWidget xml castToWindow "window"
  [tcode, ptcode, ecode, tinfo, ptinfo, einfo, log, view] <- mapM (xmlGetWidget xml castToTextView)
    ["tcode", "ptcode", "ecode", "tinfo", "ptinfo", "einfo", "log", "view"]
  input <- xmlGetWidget xml castToEntry "input"
  status <- xmlGetWidget xml castToStatusbar "status"
  return $ GUI window tcode ptcode ecode tinfo ptinfo einfo log view input status

-- ruab_ui :: Options -> BS.ByteString -> BS.ByteString -> BS.ByteString -> VarMap -> LocMap -> IO ()
-- ruab_ui opt tcode ptcode ecode varmap locmap = do
ruab_ui :: IO ()
ruab_ui = do
  gui <- loadGui
  _ <- onDestroy (guiWin gui) mainQuit
  widgetShowAll (guiWin gui)
  mainGUI
  return ()
