{-# LANGUAGE QuasiQuotes, FlexibleInstances, MultiParamTypeClasses #-}
module Ruab.Core.Test (tests) where

-- imports {{{1
import Control.Monad (forM_)
import Data.Maybe (fromJust, isJust)
import Ocram.Ruab (TRow(..), PRow(..))
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
      let
        plines = lines pcode
        ppm'   = enrich ppm
        cases  = filter (\(line, _) -> not (line =~ "^\\s*#.*$" :: Bool)) $ zip (lines code) $ map TRow [1..]
      forM_ cases $ \(line, trow) ->
        case t2p_row' ppm' trow of
          Nothing -> assertBool "non-empty row" (line =~ "^\\s*$")
          Just prow -> do
            line @=? (plines !! getPRow (prow - 1))
            let trow' = p2t_row' ppm' prow
            assertBool "back-mapping failed" (isJust trow')
            trow @=? fromJust trow'

