module Ocram.Main (main) where

import Ocram.Analysis (analysis, footprint)
import Ocram.Options (options, Options(..))
import Ocram.Output (pretty_print, write_debug_symbols, dump_pal)
import Ocram.Parser (parse)
import Ocram.Text (OcramError, show_errors)
import Ocram.Test (runTests)
import qualified Ocram.Transformation.Inline as Inline
import System.Environment (getArgs, getProgName)
import System.Exit (exitWith, ExitCode(ExitFailure))
import System.IO (stderr, hPutStrLn)

main :: IO ()
main = do
  argv <- getArgs 
  if head argv == "--test"
    then runTests (tail argv)
    else runCompiler argv
    
runCompiler :: [String] -> IO ()
runCompiler argv = do
  prg <- getProgName
  opt <- exitOnError "options" $ options prg argv 
  ast <- exitOnError "parser" =<< parse opt (optInput opt)
  ana <- exitOnError "analysis" $ analysis ast
  let (ast', ds) = Inline.transformation ana ast
  let fpr = footprint ana
  let ph = Inline.extract_pal ana ast'
  exitOnError "output" =<< dump_pal opt fpr ph
  exitOnError "output" =<< pretty_print opt ast'
  exitOnError "output" =<< write_debug_symbols opt ds
  return ()

exitOnError :: String -> Either [OcramError] a -> IO a
exitOnError module_ (Left es) = hPutStrLn stderr (show_errors module_ es) >> exitWith (ExitFailure 1)
exitOnError _ (Right x) = return x
