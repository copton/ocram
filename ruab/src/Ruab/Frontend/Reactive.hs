module Ruab.Frontend.Reactive
-- export {{{1
(
  event_input
) where

-- imports {{{1
import Control.Monad (when)
import qualified Reactive.Banana as R
import qualified Graphics.UI.Gtk as G
import qualified Graphics.UI.Gtk.Gdk.Events as G

event_input :: G.Entry -> R.NetworkDescription t (R.Event t String)
event_input entry = do
  ah <- R.liftIO $ do
    (addHandler, runHandlers) <- R.newAddHandler
    _ <- G.onKeyPress entry (eventHandler runHandlers)
    return addHandler
  R.fromAddHandler ah
    where
      eventHandler runHandlers (G.Key _ _ _ [] _ _ _ _ "Return" _) = do
        command <- G.entryGetText entry
        when (command /= "") $ runHandlers command
        G.entrySetText entry ""
        return True
      eventHandler _ _ = return False
