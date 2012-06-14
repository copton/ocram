module Ocram.Main (main) where

-- imports {{{1
import Ocram.Analysis (analysis)
import Ocram.Debug (format_debug_info)
import Ocram.Options (options)
import Ocram.IO (parse, generate_pal, dump_ptcode, dump_ecode, dump_debug_info)
import Ocram.Print (print_with_log)
import Ocram.Text (OcramError, show_errors)
import Ocram.Test (runTests)
import Ocram.Transformation (transformation)
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs, getProgName)
import System.Exit (exitWith, ExitCode(ExitFailure))
import System.IO (stderr, hPutStrLn)


main :: IO () -- {{{1
main = do
  argv <- getArgs 
  if head argv == "--test"
    then runTests (tail argv)
    else runCompiler argv
    
runCompiler :: [String] -> IO () -- {{{2
runCompiler argv = do
  prg <- getProgName
  cwd <- getCurrentDirectory
  opt <- exitOnError "options" $ options prg argv 
  (tcode, ptcode, ast) <- exitOnError "parser" =<< parse opt
  (cg, fpr) <- exitOnError "analysis" $ analysis ast
  let (ast', pal, varMap) = transformation cg ast
  let (ecode, locMap) = print_with_log ast'
  let debuginfo = format_debug_info opt cwd tcode ptcode ecode locMap varMap
  exitOnError "output" =<< generate_pal opt fpr pal
  exitOnError "output" =<< dump_ptcode opt ptcode
  exitOnError "output" =<< dump_ecode opt ecode
  exitOnError "output" =<< dump_debug_info opt debuginfo
  return ()

exitOnError :: String -> Either [OcramError] a -> IO a -- {{{2
exitOnError module_ (Left es) = hPutStrLn stderr (show_errors module_ es) >> exitWith (ExitFailure 1)
exitOnError _ (Right x) = return x
