{-# LANGUAGE QuasiQuotes #-}
module Ruab.Mapping.Test (tests) where

-- imports {{{1
import Control.Monad (forM_)
import Ruab.Mapping (map_preprocessed_row)
import Ruab.Mapping.Internal (preproc_map)
import Ruab.Test.Lib (enumTestGroup, paste)
import System.Exit (ExitCode(ExitSuccess))
import System.IO (hPutStr, hClose, hGetContents)
import System.Process (createProcess, StdStream(CreatePipe), waitForProcess, proc, std_out, std_in)
import Test.Framework (Test, testGroup)
import Test.HUnit (assertEqual, Assertion, assertBool)
import Text.Regex.Posix ((=~))

import qualified Data.ByteString.Char8 as BS

tests :: Test -- {{{2
tests = testGroup "Mapping" [test_map_preprocessed_row]

test_map_preprocessed_row :: Test -- {{{1
test_map_preprocessed_row = enumTestGroup "map_preprocessed_row" $ map runTest [
    [paste|
      #include <stdio.h>
      int main() {
        printf("hello world\n");
        return 0;
      }
    |]
  , [paste|
      #include <stdio.h>
      void foo() { }
      #include <string.h>
      int main() {
        printf("hello world\n");
        return 0;
      }
    |]
  ]
  where
    cpp :: String -> IO String
    cpp input = do
      (Just hin, Just hout, _, hproc) <- createProcess (proc "gcc" ["-xc", "-E", "-o-", "-"]) {std_out = CreatePipe, std_in = CreatePipe}
      hPutStr hin input
      hClose hin
      exitCode <- waitForProcess hproc
      case exitCode of
        ExitSuccess -> hGetContents hout
        _ -> error $ "calling pre-processor failed: " ++ show exitCode

    runTest :: String -> Assertion
    runTest code = do
      pcode <- cpp code
      let plines = lines pcode
      let ppm = preproc_map $ BS.pack pcode
      let cases = filter (\(line, _) -> not (line =~ "^\\s*#.*$" :: Bool)) $ zip (lines code) [1..]
      forM_ cases $ \(line, row) -> 
        case map_preprocessed_row ppm row of
          Nothing -> assertBool ("row: " ++ show row ++ "/" ++ line) (line =~ "^\\s*$")
          Just row' ->
            assertEqual ("rows: " ++ show row ++ "/" ++ show row') line (plines !! (row' - 1))

