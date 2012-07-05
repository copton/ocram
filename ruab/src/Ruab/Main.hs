module Ruab.Main 
(
  main
) where

-- imports {{{1
import Ruab.Options (options)
import Ruab.Test (runTests)
import System.Environment (getArgs, getProgName)
import System.Exit (exitWith, ExitCode(ExitFailure))
import System.IO (stderr, hPutStrLn)

import qualified Ruab.Frontend as F

main :: IO () -- {{{1
main = do
  argv <- getArgs
  case argv of
    ("--test":rest) -> runTests rest
    _ -> runDebugger argv

runDebugger :: [String] -> IO ()
runDebugger argv = do
  prg <- getProgName 
  opt <- either exit return $ options prg argv
  F.run opt
  return ()
  where
    exit (ec, why) = hPutStrLn stderr why >> exitWith ec
