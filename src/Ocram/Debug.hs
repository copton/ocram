module Ocram.Debug (debug) where

import Ocram.Context
import Data.Map as Map

debug :: Context -> IO ()
debug ctx = do
	putStrLn "DEBUG ###"
	putStrLn $ show $ Map.keys $ ctxBlockingFunctions ctx
	putStrLn "### DEBUG"
	return ()
