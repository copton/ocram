module Ruab.Main 
(
  main
) where

-- imports {{{1
import Ocram.Ruab
import Ruab.Backend
import Ruab.Debug (load_debug_info)
import Ruab.Frontend (ruab_ui)
import Ruab.Mapping
import Ruab.Options (options)
import Ruab.Test (runTests)
import System.Environment (getArgs, getProgName)
import System.Exit (exitWith, ExitCode)
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
  opt <- exitOnError $ options prg argv
  di <- exitOnError =<< load_debug_info opt
  return ()

exitOnError :: Either (ExitCode, String) a -> IO a -- {{{2
exitOnError (Left (ec, why)) = hPutStrLn stderr why >> exitWith ec
exitOnError (Right x) = return x
