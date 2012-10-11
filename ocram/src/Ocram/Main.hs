module Ocram.Main (main) where

-- imports {{{1
import Ocram.Analysis (analysis, Analysis(anaCritical, anaBlocking, anaCallgraph), footprint)
import Ocram.Backend (tcode_2_ecode)
import Ocram.Debug (create_debug_info)
import Ocram.Intermediate (ast_2_ir)
import Ocram.Options (options)
import Ocram.IO (parse, generate_pal, dump_ecode, dump_debug_info)
import Ocram.Print (render_with_log')
import Ocram.Ruab (encode_debug_info)
import Ocram.Text (OcramError, show_errors)
import Ocram.Test (runTests)
import System.Directory (getCurrentDirectory)
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
  prg                   <- getProgName
  cwd                   <- getCurrentDirectory

  opt                   <- exitOnError "options"  $ options prg cwd argv 
  (tcode, pcode, tAst)  <- exitOnError "parser" =<< parse opt
  ana                   <- exitOnError "analysis" $ analysis tAst

  let cfs                = ast_2_ir (anaBlocking ana) (anaCritical ana)
  let (eAst, pal, vm)    = tcode_2_ecode ana cfs
  let (ecode, bps)       = render_with_log' eAst
  let di                 = encode_debug_info $ create_debug_info opt tcode pcode ana vm bps ecode

  exitOnError "output" =<< generate_pal opt (footprint (anaCallgraph ana)) pal
  exitOnError "output" =<< dump_ecode opt ecode
  exitOnError "output" =<< dump_debug_info opt di

  return ()

exitOnError :: String -> Either [OcramError] a -> IO a -- {{{2
exitOnError module_ (Left es) = hPutStrLn stderr (show_errors module_ es) >> exitWith (ExitFailure 1)
exitOnError _ (Right x) = return x

_tests :: IO ()  -- {{{1
_tests = tests []

tests :: [String] -> IO () -- {{{1
tests args = runTests $ words "--hide-successes --plain  -j 3" ++ args
