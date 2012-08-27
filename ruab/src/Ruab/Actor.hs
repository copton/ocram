module Ruab.Actor
-- exports {{{1
(
    new_actor
  , update
 , quit, wait, kill
) where

-- imports {{{1
import Control.Concurrent.STM (atomically, TChan, newTChanIO, writeTChan, readTChan)
import Control.Concurrent (forkIO, killThread, ThreadId, MVar, newEmptyMVar, takeMVar, putMVar)
import Control.Exception.Base (SomeException, catch)
import Prelude hiding (catch)

data Actor a = Actor {
    actorThread :: ThreadId
  , actorInbox  :: TChan (Either () (Update a))
  , actorQuit   :: MVar (Quit a)
  }

type Update a = a -> IO a
type Quit a = Either SomeException a

new_actor :: a -> IO (Actor a)
new_actor s0 = do
  inbox <- newTChanIO
  quitFlag <- newEmptyMVar
  thread <- forkIO $ actor quitFlag inbox s0
  return $ Actor thread inbox quitFlag

send :: Actor a -> Either () (Update a) -> IO ()
send a o = atomically $ writeTChan (actorInbox a) o

update :: Actor a -> Update a -> IO ()
update a u = send a (Right u)

quit :: Actor a -> IO ()
quit a = send a (Left ())

wait :: Actor a -> IO (Quit a)
wait = takeMVar . actorQuit

kill :: Actor a -> IO ()
kill = killThread . actorThread

actor :: MVar (Quit a) -> TChan (Either () (Update a)) -> a -> IO ()
actor quitFlag inbox s0 = loop s0 `catch` handle
  where
    loop s = do
      message <- atomically $ readTChan inbox
      case message of
        Left () -> putMVar quitFlag (Right s)
        Right f -> f s >>= loop

    handle :: SomeException -> IO ()
    handle e = putMVar quitFlag (Left e)
