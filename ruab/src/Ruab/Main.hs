module Ruab.Main 
(
  main
) where

-- imports {{{1
import Ruab.Frontend (ruab_ui)
import Ruab.Mapping
import Ruab.Backend
import Ruab.Options
import Ruab.Test (runTests)
import System.Environment (getArgs)

main :: IO () -- {{{1
main = do
  argv <- getArgs
  case head argv of
    "--test" -> runTests (tail argv)
    _ -> runDebugger argv

runDebugger :: [String] -> IO ()
runDebugger _ = ruab_ui
