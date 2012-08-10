module Ocram.Main (main, tests) where

-- imports {{{1
import Ocram.Analysis (analysis)
import Ocram.Debug (create_debug_info)
import Ocram.Options (options)
import Ocram.IO (parse, generate_pal, dump_ecode, dump_debug_info)
import Ocram.Print (print_with_log)
import Ocram.Ruab (encode_debug_info)
import Ocram.Text (OcramError, show_errors)
import Ocram.Test (runTests)
import Ocram.Transformation (transformation)
import System.Environment (getArgs, getProgName)
import System.Exit (exitWith, ExitCode(ExitFailure))
import System.IO (stderr, hPutStrLn)

main :: IO () -- {{{1
main = do
  argv <- getArgs 
  case argv of
    ("--test":rest) -> runTests rest
    _ -> runCompiler argv

runCompiler :: [String] -> IO () -- {{{1
runCompiler argv = do
  prg <- getProgName
  opt <- exitOnError "options" $ options prg argv 
  (tcode, pcode, ast) <- exitOnError "parser" =<< parse opt
  (cg, fpr) <- exitOnError "analysis" $ analysis ast
  let (ast', pal, vm) = transformation cg ast
  let (ecode, lm, bl) = print_with_log ast'
  let di = encode_debug_info $ create_debug_info opt cg tcode pcode ecode vm lm bl
  exitOnError "output" =<< generate_pal opt fpr pal
  exitOnError "output" =<< dump_ecode opt ecode
  exitOnError "output" =<< dump_debug_info opt di
  return ()

exitOnError :: String -> Either [OcramError] a -> IO a -- {{{2
exitOnError module_ (Left es) = hPutStrLn stderr (show_errors module_ es) >> exitWith (ExitFailure 1)
exitOnError _ (Right x) = return x

tests :: IO ()  -- {{{1
tests = testsWith []

testsWith :: [String] -> IO () -- {{{1
testsWith args = runTests $ words "--hide-successes --plain  -j 3" ++ args
