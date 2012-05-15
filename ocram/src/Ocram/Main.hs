module Ocram.Main (main) where

import Ocram.Analysis (analysis)
import Ocram.Options (options, Options(..))
import Ocram.Output (generate_pal, dump_ecode, dump_debug_info)
import Ocram.Print (print_with_log)
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
  (cg, fpr) <- exitOnError "analysis" $ analysis ast
  let (ast', pal, varMap) = Inline.transformation cg ast
  let (ecode, locMap) = print_with_log ast'
  exitOnError "output" =<< generate_pal opt fpr pal
  exitOnError "output" =<< dump_ecode opt ecode
  exitOnError "output" =<< dump_debug_info opt locMap varMap
  return ()

exitOnError :: String -> Either [OcramError] a -> IO a
exitOnError module_ (Left es) = hPutStrLn stderr (show_errors module_ es) >> exitWith (ExitFailure 1)
exitOnError _ (Right x) = return x
