{-# LANGUAGE QuasiQuotes, FlexibleInstances, MultiParamTypeClasses, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Ruab.Core.Test (tests) where

-- imports {{{1
import Control.Exception.Base (throwIO, try, SomeException)
import Control.Monad.Fix (mfix)
import Control.Monad (forM_, when)
import Data.List (intercalate)
import Data.Maybe (fromJust, isJust)
import Ocram.Ruab (TRow(..), PRow(..))
import Ruab.Actor (new_actor, update, wait, quit)
import Ruab.Core.Internal (t2p_row', p2t_row')
import Ruab.Core (Result(..), Status(..), Command(..), setup, create_network, UserBreakpoint(..))
import Ruab.Options (Options(..))
import Ruab.Test.Lib (enumTestGroup, paste, TestData(..), enrich, TPreprocMap)
import System.Directory (setCurrentDirectory)
import System.Exit (ExitCode(ExitSuccess))
import System.IO (hPutStr, hClose, hGetContents)
import System.Process (createProcess, StdStream(CreatePipe), waitForProcess, proc, std_out, std_in)
import Test.Framework (Test, testGroup)
import Test.HUnit (Assertion, assertBool, (@?), (@=?), assertFailure)
import Text.Regex.Posix ((=~))

tests :: Test -- {{{1
tests = testGroup "Core" [test_tp_row_mapping, test_integration]

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

-- test integration {{{1
data ExpectInput
  = ExpectResponse (Either String Result)
  | ExpectStatus Status
  deriving Show

data Expect -- {{{2
  = NextResponse (Either String Result) (Maybe Command)
  | NextStatus (Status -> Bool) (Maybe Command)
  | Choice [Expect]

instance Show Expect where
  show (NextResponse x _) = "NextResponse " ++ show x
  show (NextStatus _ _) = "NextStatus"
  show (Choice es) = intercalate ", " $ map show es

type ExpectScript = (Maybe Command, [Expect]) -- {{{2

deriving instance Show Result
deriving instance Show Command
deriving instance Eq UserBreakpoint
deriving instance Eq Result

test_integration :: Test -- {{{2
test_integration = enumTestGroup "integration" $ map runTest [
    (Just CmdRun, [
        NextStatus (const True) Nothing
      , NextStatus (const True) Nothing
      , NextResponse (Right ResStart) (Just CmdShutdown)
      , NextStatus (const True) Nothing
      , NextResponse (Right ResShutdown) Nothing
      , NextStatus (const True) Nothing
      ])
  ]
  where
    runTest :: ExpectScript -> Assertion -- {{{2
    runTest (command, future) = do
      setCurrentDirectory "test"
      let options = Options "debug.json" "ec.elf" Nothing False 
      core <- setup options
      actor <- new_actor future
      fCommand <- mfix(\fCommand' -> create_network core options (fResponse actor fCommand') (fStatus actor fCommand'))
      when (isJust command) (fCommand (fromJust command))
      result <- wait actor
      case result of
        Left e -> throwIO e
        Right _ -> return ()

    fResponse actor fCommand response = update actor $ step actor fCommand (ExpectResponse (snd response))
    fStatus actor fCommand status = update actor $ step actor fCommand (ExpectStatus status)

    step _ _ input [] = do
      assertFailure $ "incomplete script: " ++ show input
      fail ""

    step actor fCommand input (expected:rest) = do
--      print $ "<- " ++ show input
      cmd <- handle input expected
--      print $ "-> " ++ show cmd
      when (isJust cmd) (fCommand (fromJust cmd))
      when (null rest) (quit actor)
      return rest

    handle :: ExpectInput -> Expect -> IO (Maybe Command)
    handle input (Choice alternatives) = do
      res <- mapM (try . handle input) $ alternatives
      inspect res []
        where
          inspect (Right cmd:_) _ = return cmd
          inspect (Left e: rest) es = inspect rest (show (e :: SomeException) : es)
          inspect [] es = do
            assertFailure $ "no viable alternative found: " ++ show es
            fail ""

    handle (ExpectStatus status) (NextStatus f cmd) = do
      f status @? "unexpected status"
      return cmd

    handle (ExpectResponse response) (NextResponse response' cmd) = do
      response @=? response'
      return cmd

    handle expect input = do
      assertFailure $ "unexpected input: " ++ show expect ++ " / " ++ show input
      fail ""
