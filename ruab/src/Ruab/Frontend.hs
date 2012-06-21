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

import qualified Data.ByteString.Char8 as BS
import Debug.Trace (trace)

data GUI = GUI { -- {{{1
    guiWin :: Window
  , guiTcode :: TextView
  , guiPtcode :: TextView
  , guiEcode :: TextView
  , guiOutput :: TextView
  , guiInput :: Entry
  , guiStatus :: Statusbar
  }


loadGui :: IO GUI
loadGui = do
  gladefn <- getDataFileName "ruab.glade"
  _ <- initGUI
  trace (gladefn) (return ())
  Just xml <- xmlNew gladefn
  win' <- xmlGetWidget xml castToWindow "winMain"
  [tcode', ptcode', ecode', output'] <- mapM (xmlGetWidget xml castToTextView) ["txtTcode", "txtPTcode", "txtEcode", "txtOutput"]
  input' <- xmlGetWidget xml castToEntry "input"
  status' <- xmlGetWidget xml castToStatusbar "status"
  return $ GUI win' tcode' ptcode' ecode' output' input' status'

-- ruab_ui :: Options -> BS.ByteString -> BS.ByteString -> BS.ByteString -> VarMap -> LocMap -> IO ()
-- ruab_ui opt tcode ptcode ecode varmap locmap = do
ruab_ui :: IO ()
ruab_ui = do
  gui <- loadGui
  _ <- onDestroy (guiWin gui) mainQuit
  widgetShowAll (guiWin gui)
  mainGUI
  return ()
