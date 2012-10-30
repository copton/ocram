{-# LANGUAGE QuasiQuotes #-}
module Ocram.Debug.Test (tests) where

-- imports {{{1
import Control.Monad (forM_)
import Data.Maybe (fromJust, isJust)
import Ocram.Debug.DebugInfo
import Ocram.Ruab (TRow(..), PRow(..), t2p_row, p2t_row)
import Ocram.Test.Lib (enumTestGroup, paste, reduce, TMapTP)
import System.Exit (ExitCode(ExitSuccess))
import System.IO (hPutStr, hClose)
import System.Process (createProcess, StdStream(CreatePipe), waitForProcess, proc, std_out, std_in)
import Test.Framework (Test, testGroup)
import Test.HUnit ((@=?), Assertion, assertBool)
import Text.Regex.Posix ((=~))

import qualified Data.ByteString.Char8 as BS

tests :: Test -- {{{1
tests = testGroup "Debug" [test_t2p_map]

test_t2p_map :: Test -- {{{1
test_t2p_map = enumTestGroup "t2p_map" $ map runTest [
    ([paste|
      #include <stdio.h>
      int main() {
        printf("hello world\n");
        return 0;
      }
    |], (7, 852, [(1,4),(3,848)]))
  , ([paste|
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
    cpp :: String -> IO BS.ByteString
    cpp tcode = do
      (Just hin, Just hout, _, hproc) <- createProcess (proc "gcc" ["-xc", "-E", "-o-", "-"]) {std_out = CreatePipe, std_in = CreatePipe}
      hPutStr hin tcode 
      hClose hin
      exitCode <- waitForProcess hproc
      case exitCode of
        ExitSuccess -> BS.hGetContents hout
        _ -> error $ "calling pre-processor failed: " ++ show exitCode

    runTest :: (String, TMapTP) -> Assertion
    runTest (tcode, expMtp) = do
      pcode <- cpp tcode
      let resMtp = t2p_map (BS.pack tcode) pcode 
      expMtp @=? reduce resMtp
      
      let
        cases  = filter (\(line, _) -> not (line =~ "^\\s*#.*$" :: Bool)) $ zip (lines tcode) $ map TRow [1..]
        plines = lines (BS.unpack pcode)
      forM_ cases $ \(line, trow) ->
        case t2p_row resMtp trow of
          Nothing -> assertBool "non-empty row" (line =~ "^\\s*$")
          Just prow -> do
            line @=? (plines !! getPRow (prow - 1))
            let trow' = p2t_row resMtp prow
            assertBool "back-mapping failed" (isJust trow')
            trow @=? fromJust trow'
