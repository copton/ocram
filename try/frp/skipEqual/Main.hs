module Main where

import Reactive.Banana
import Control.Concurrent (threadDelay)

data Command = CommandA | CommandB

type State = Int

createNetwork :: EventSource Command -> NetworkDescription t ()
createNetwork ah = do
  eCommand <- fromAddHandler (addHandler ah)
  let
    eState = accumE 0 (handleCommand <$> eCommand)
    eStateUpdate = skipEqual eState

  reactimate $ putStrLn . show <$> eStateUpdate

handleCommand :: Command -> State -> State
handleCommand CommandA = (+1)
handleCommand CommandB = id

type EventSource a = (AddHandler a, a -> IO ())

addHandler :: EventSource a -> AddHandler a
addHandler = fst

fire :: EventSource a -> a -> IO ()
fire = snd

-- skipEqual :: Eq a => Event t a -> Event t a
-- skipEqual = filterJust . fst . mapAccum Nothing . fmap f
--     where
--     f y (Just x) = if x == y then (Nothing,Just x) else (Just y,Just y)
--     f y Nothing  = (Just y, Just y)

skipEqual :: Eq a => Event t a -> Event t a
skipEqual = filterJust . fmap snd . accumE (Nothing, Nothing) . fmap f
  where
    f y (Nothing, _) = (Just y, Just y)
    f y (Just x, Nothing)
      | y == x = (Just x, Nothing)
      | otherwise = (Just y, Just y)
    f y (_, Just z)
      | y == z = (Just z, Nothing)
      | otherwise = (Just z, Just z)

-- skipEqual :: Eq a => Event t a -> Event t a
-- skipEqual = filterJust . accumE Nothing . fmap f

-- f :: Eq a => a -> (Maybe a -> Maybe a)
-- f y (Just x)
--   | x == y = Nothing
--   | otherwise = Just y
-- f y Nothing = Just y

eventLoop :: EventSource Command -> IO ()
eventLoop ah = loop
  where
    loop = do
      fire ah CommandA
      threadDelay $ 5^6
      fire ah CommandB
      threadDelay $ 5^6
      fire ah CommandB
      threadDelay $ 5^6
      loop

main :: IO ()
main = do
  ah <- newAddHandler
  network <- compile $ createNetwork ah
  actuate network
  eventLoop ah
