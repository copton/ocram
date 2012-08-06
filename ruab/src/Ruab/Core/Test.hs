{-# LANGUAGE QuasiQuotes, FlexibleInstances, MultiParamTypeClasses #-}
module Ruab.Core.Test (tests) where

-- imports {{{1
import Control.Monad (forM_)
import Data.Maybe (fromJust, isJust)
import Ruab.Core.Internal (t2p_row', p2t_row')
import Ruab.Test.Lib (enumTestGroup, paste, TestData(..), enrich, TPreprocMap)
import System.Exit (ExitCode(ExitSuccess))
import System.IO (hPutStr, hClose, hGetContents)
import System.Process (createProcess, StdStream(CreatePipe), waitForProcess, proc, std_out, std_in)
import Test.Framework (Test, testGroup)
import Test.HUnit (Assertion, assertBool, (@=?))
import Text.Regex.Posix ((=~))

tests :: Test -- {{{1
tests = testGroup "Debug" [test_tp_row_mapping]

test_tp_row_mapping :: Test -- {{{1
test_tp_row_mapping = enumTestGroup "t/p row mapping" $ map runTest [
    -- {{{2
    ([paste|
      #include <stdio.h>
      int main() {
        printf("hello world\n");
        return 0;
      }
    |], (7, 852, [(1,4),(3,848)]))
  , -- {{{2
    ([paste|
      #include <stdio.h>
      void foo() { }
      #include <string.h>
      int main() {
        printf("hello world\n");
        return 0;
      }
    |], (9, 1110, [(1,4),(3,848),(5,1106)]))
  ]
  where
    cpp :: String -> IO String -- {{{2
    cpp input = do
      (Just hin, Just hout, _, hproc) <- createProcess (proc "gcc" ["-xc", "-E", "-o-", "-"]) {std_out = CreatePipe, std_in = CreatePipe}
      hPutStr hin input
      hClose hin
      exitCode <- waitForProcess hproc
      case exitCode of
        ExitSuccess -> hGetContents hout
        _ -> error $ "calling pre-processor failed: " ++ show exitCode

    runTest :: (String, TPreprocMap) -> Assertion -- {{{2
    runTest (code, ppm) = do
      pcode <- cpp code
      let plines = lines pcode
      let ppm' = enrich ppm
      let cases = filter (\(line, _) -> not (line =~ "^\\s*#.*$" :: Bool)) $ zip (lines code) [1..]
      forM_ cases $ \(line, row) ->
        case t2p_row' ppm' row of
          Nothing -> assertBool "non-empty row" (line =~ "^\\s*$")
          Just row' -> do
            line @=? (plines !! (row' - 1))
            
            let row'' = p2t_row' ppm' row'
            assertBool "back-mapping failed" (isJust row'')
            row @=? fromJust row''

