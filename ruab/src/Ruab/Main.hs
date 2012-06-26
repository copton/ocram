module Ruab.Main 
(
  main
) where

-- imports {{{1
import Ruab.Frontend (ruab_ui)
import Ruab.Mapping (load_context)
import Ruab.Options (options)
import Ruab.Test (runTests)
import System.Environment (getArgs, getProgName)
import System.Exit (exitWith, ExitCode)
import System.IO (stderr, hPutStrLn)

import Ruab.Backend

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
  ctx <- exitOnError =<< load_context opt
  ruab_ui opt ctx

exitOnError :: Either (ExitCode, String) a -> IO a -- {{{2
exitOnError (Left (ec, why)) = hPutStrLn stderr why >> exitWith ec
exitOnError (Right x) = return x
