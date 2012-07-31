{-# LANGUAGE TupleSections #-}

module Main where

import Reactive.Banana
import Control.Concurrent (threadDelay)

data Command = CommandNop | CommandA | CommandB deriving (Show, Enum)

type State = Int

-- createNetwork :: AddHandler Command -> NetworkDescription t ()
-- createNetwork ah = do
--   eCommand <- fromAddHandler ah
--   (eStateUpdate, fStateUpdate) <- newEvent
--   let
--     bState = accumB 0 eStateUpdate
--     bHandleCommand = handleCommand fStateUpdate <$> bState
--     eReaction = bHandleCommand <@> eCommand

--   reactimate eReaction

createNetwork :: AddHandler Command -> NetworkDescription t ()
createNetwork ah = do
  eCommand <- fromAddHandler ah
  (eStateUpdate, fStateUpdate) <- newEvent
  reactimate $ (handleCommand fStateUpdate <$> (accumB 0 eStateUpdate)) <@> eCommand

handleCommand :: ((State -> State) -> IO ()) -> State -> Command -> IO ()
handleCommand f s c = putStrLn ("state: " ++ show s ++ ", command: " ++ show c) >> f (+1)

eventLoop :: (Command -> IO ()) -> IO ()
eventLoop fire = loop
  where
    loop = do
      fire CommandA
      threadDelay $ 5^6
      fire CommandB
      threadDelay $ 5^6
      loop

main :: IO ()
main = do
  (ah, fire) <- newAddHandler
  network <- compile $ createNetwork ah
  actuate network
  eventLoop fire
