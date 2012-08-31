{-# LANGUAGE QuasiQuotes #-}
module Ocram.Debug.Test (tests) where

-- imports {{{1
import Control.Arrow ((***))
import Language.C.Syntax.AST (CTranslUnit)
import Ocram.Debug.Internal (preproc_map, fun_map)
import Ocram.Ruab (PreprocMap(..), TRow(..), PRow(..))
import Ocram.Test.Lib (enumTestGroup, paste, enrich, reduce, TFunMap)
import System.Exit (ExitCode(ExitSuccess))
import System.IO (hPutStr, hClose)
import System.Process (createProcess, StdStream(CreatePipe), waitForProcess, proc, std_out, std_in)
import Test.Framework (Test, testGroup)
import Test.HUnit ((@=?), Assertion)

import qualified Data.ByteString.Char8 as BS

tests :: Test -- {{{1
tests = testGroup "Debug" [test_preproc_map, test_fun_map]

test_preproc_map :: Test -- {{{1
test_preproc_map = enumTestGroup "preproc_map" $ map runTest [
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

    runTest :: (String, (Int, Int, [(Int, Int)])) -> Assertion
    runTest (tcode, (maxTRow, maxPRow, mapping)) = do
      pcode <- cpp tcode
      let ppm = preproc_map (BS.pack tcode) pcode 
      TRow maxTRow @=? ppmMaxTRow ppm
      PRow maxPRow @=? ppmMaxPRow ppm
      map (TRow *** PRow) mapping @=? ppmMapping ppm

test_fun_map :: Test -- {{{1
test_fun_map = enumTestGroup "fun_map" $ map runTest [
    ([paste|
      void foo() {

      }
    |], [
      ("foo", 2, 4)
    ])
  ,
    ([paste|
      void foo() {

      }
      void bar() {

      }
    |], [
      ("foo", 2, 4)
    , ("bar", 5, 7)
    ])
  ]
  where
    runTest :: (String, TFunMap) -> Assertion
    runTest (inputCode, expectedFunMap) =
      let
        ast = enrich inputCode :: CTranslUnit
        resultFunMap = reduce $ fun_map ast
      in
        expectedFunMap @=? resultFunMap
