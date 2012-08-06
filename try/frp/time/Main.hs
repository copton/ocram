module Main where

import Reactive.Banana
import Control.Concurrent (threadDelay)
import Control.Monad (when)

main :: IO ()
main = do
  (ah, fire) <- newAddHandler
  let
    createNetwork = do
      eCommand <- fromAddHandler ah
      reactimate $ handle <$> eCommand
    handle n = print n >> when (n == 1) (fire 3)
    loop = threadDelay (10^6) >> loop

  network <- compile $ createNetwork
  actuate network
  fire 1
  fire 2
  loop
