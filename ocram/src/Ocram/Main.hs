module Ocram.Main (main) where

-- imports {{{1
import Ocram.Analysis (analysis)
import Ocram.Options (options)
import Ocram.IO (parse, generate_pal, dump_ecode)
import Ocram.Print (print_with_log)
import Ocram.Text (OcramError, show_errors)
import Ocram.Test (runTests)
import Ocram.Transformation (transformation)
import System.Environment (getArgs, getProgName)
import System.Exit (exitWith, ExitCode(ExitFailure))
import System.IO (stderr, hPutStrLn)


main :: IO () -- {{{1
main = do
  argv <- getArgs 
  case head argv of
    "--test" -> runTests (tail argv)
    _ -> runCompiler argv

runCompiler :: [String] -> IO () -- {{{2
runCompiler argv = do
  prg <- getProgName
  opt <- exitOnError "options" $ options prg argv 
  (_, _, ast) <- exitOnError "parser" =<< parse opt
  (cg, fpr) <- exitOnError "analysis" $ analysis ast
  let (ast', pal, _) = transformation cg ast
  let (ecode, _) = print_with_log ast'
  exitOnError "output" =<< generate_pal opt fpr pal
  exitOnError "output" =<< dump_ecode opt ecode
  return ()

exitOnError :: String -> Either [OcramError] a -> IO a -- {{{2
exitOnError module_ (Left es) = hPutStrLn stderr (show_errors module_ es) >> exitWith (ExitFailure 1)
exitOnError _ (Right x) = return x
