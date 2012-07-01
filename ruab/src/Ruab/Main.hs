module Ruab.Main 
(
  main
) where

-- imports {{{1
import Ruab.Frontend (frontend_run)
import Ruab.Options (options)
import Ruab.Test (runTests)
import Ruab.Util (save)
import System.Environment (getArgs, getProgName)
import System.Exit (exitWith, ExitCode(ExitFailure))
import System.IO (stderr, hPutStrLn)

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
  _ <- (either exit' return) =<< (save $ frontend_run opt)
  return ()
  where
    exit (ec, why) = hPutStrLn stderr why >> exitWith ec
    exit' s = exit (ExitFailure 1, s)
