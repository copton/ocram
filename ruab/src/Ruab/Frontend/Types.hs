module Ruab.Frontend.Types where

import Data.IORef (IORef)
import Graphics.UI.Gtk
import Ruab.Core (Core)
import Ruab.Frontend.Infos (InfoInstance)

data Component = Component { -- {{{1
    compCode  :: TextView
  , compInfo  :: TextView
  , compLines :: TextView
  , compLabel :: Label
  , compView  :: Viewport
  , compInfos :: IORef [InfoInstance]
  }

data GUI = GUI { -- {{{1
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
