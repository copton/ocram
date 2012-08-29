module Ruab.Actor
-- exports {{{1
(
    Actor, Quit, Update
  , new_actor
  , update
 , quit, wait, kill
) where

-- imports {{{1
import Control.Concurrent.STM (atomically, TChan, newTChanIO, writeTChan, readTChan)
import Control.Concurrent (forkIO, killThread, ThreadId, MVar, newEmptyMVar, takeMVar, putMVar)
import Control.Exception.Base (SomeException, catch)
import Prelude hiding (catch)

-- types -- {{{1
data Actor a = Actor { -- {{{2
    actorThread :: ThreadId
  , actorInbox  :: TChan (Either () (Update a))
  , actorQuit   :: MVar (Quit a)
  }

type Update a = a -> IO a -- {{{2

type Quit a = Either SomeException a -- {{{2

new_actor :: a -> IO (Actor a) -- {{{1
new_actor s0 = do
  inbox <- newTChanIO
  quitFlag <- newEmptyMVar
  thread <- forkIO $ actor quitFlag inbox s0
  return $ Actor thread inbox quitFlag

update :: Actor a -> Update a -> IO () -- {{{1
update a u = send a (Right u)

quit :: Actor a -> IO () -- {{{1
quit a = send a (Left ())

wait :: Actor a -> IO (Quit a) -- {{{1
wait = takeMVar . actorQuit

kill :: Actor a -> IO () -- {{{1
kill = killThread . actorThread

-- impl {{{1
send :: Actor a -> Either () (Update a) -> IO () -- {{{2
send a o = atomically $ writeTChan (actorInbox a) o

actor :: MVar (Quit a) -> TChan (Either () (Update a)) -> a -> IO () -- {{{2
actor quitFlag inbox s0 = loop s0 `catch` handle
  where
    loop s = do
      message <- atomically $ readTChan inbox
      case message of
        Left () -> putMVar quitFlag (Right s)
        Right f -> f s >>= loop

    handle :: SomeException -> IO ()
    handle e = putMVar quitFlag (Left e)
