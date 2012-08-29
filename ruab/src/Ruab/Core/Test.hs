{-# LANGUAGE QuasiQuotes, FlexibleInstances, MultiParamTypeClasses, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Ruab.Core.Test (tests) where

-- imports {{{1
import Control.Exception.Base (throwIO, try, SomeException)
import Control.Monad.Fix (mfix)
import Control.Monad (forM_, when)
import Data.List (intercalate, find)
import Data.Maybe (fromJust, isJust)
import Ruab.Actor (new_actor, update, wait, quit)
import Ruab.Core.Internal (t2p_row', p2t_row')
import Ruab.Core
import Ruab.Options (Options(..))
import Ruab.Test.Lib (enumTestGroup, paste, TestData(..), enrich, TPreprocMap)
import System.Exit (ExitCode(ExitSuccess))
import System.IO (hPutStr, hClose, hGetContents)
import System.Process (createProcess, StdStream(CreatePipe), waitForProcess, proc, std_out, std_in)
import Test.Framework (Test, testGroup)
import Test.HUnit (Assertion, assertBool, (@?), (@=?), assertFailure)
import Text.Printf (printf)
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
-- types {{{2
data ExpectInput -- {{{3
  = InputResponse (Either String Result)
  | InputStatus Status
  deriving Show

data Expect -- {{{3
  = ExpectStart Command
  | ExpectResponse (Either String Result) (Maybe Command)
  | ExpectStatus [(Status -> Bool)] (Maybe Command)
  | Choice [Expect]

type ExpectScript = [Expect] -- {{{3

-- instances {{{2
instance Show Expect where
  show (ExpectStart x ) = "ExpectStart " ++ show x
  show (ExpectResponse x _) = "ExpectResponse " ++ show x
  show (ExpectStatus _ _) = "ExpectStatus"
  show (Choice es) = intercalate ", " $ map show es


deriving instance Show Result
deriving instance Show Command
deriving instance Eq UserBreakpoint
deriving instance Eq Result

test_integration :: Test -- {{{2
test_integration = enumTestGroup "integration" $ map runTest [
     -- start and quit {{{3
      [ ExpectStart CmdRun
      , ExpectStatus [isWaiting] Nothing
      , ExpectResponse (Right ResRun) (Just CmdShutdown)
      , ExpectStatus [isRunning] Nothing
      , ExpectResponse (Right ResShutdown) Nothing
      , ExpectStatus [isShutdown] Nothing
      ]
     -- break point in critical function {{{3
    , [ ExpectStart $ CmdAddBreakpoint (PRow 515) []
      , ExpectStatus [isWaiting] Nothing
      , ExpectResponse (Right (ResAddBreakpoint (UserBreakpoint 1 (PRow 515) [0, 1]))) (Just CmdRun)
      , ExpectResponse (Right ResRun) Nothing
      , ExpectStatus [isRunning] Nothing
      , ExpectStatus [isStopped, threadStopped 0 1 515] (Just CmdShutdown)
      , ExpectResponse (Right ResShutdown) Nothing
      , ExpectStatus [isShutdown] Nothing
      ]
     -- break point with condition in critical function {{{3
    , [ ExpectStart $ CmdAddBreakpoint (PRow 515) [1]
      , ExpectStatus [isWaiting] Nothing
      , ExpectResponse (Right (ResAddBreakpoint (UserBreakpoint 1 (PRow 515) [1]))) (Just CmdRun)
      , ExpectResponse (Right ResRun) Nothing
      , ExpectStatus [isRunning] Nothing
      , ExpectStatus [isStopped, threadStopped 1 1 515] (Just CmdShutdown)
      , ExpectResponse (Right ResShutdown) Nothing
      , ExpectStatus [isShutdown] Nothing
      ]
    -- break point in non-critical function {{{3
    , [ ExpectStart $ CmdAddBreakpoint (PRow 461) []
      , ExpectStatus [isWaiting] Nothing
      , ExpectResponse (Right (ResAddBreakpoint (UserBreakpoint 1 (PRow 461) [0, 1, 2]))) (Just CmdRun)
      , ExpectResponse (Right ResRun) Nothing
      , ExpectStatus [isRunning] Nothing
      , ExpectStatus [isStopped, threadStopped 2 1 461] (Just CmdShutdown)
      , ExpectResponse (Right ResShutdown) Nothing
      , ExpectStatus [isShutdown] Nothing
      ]
    -- "step" over critical function call {{{3
    , [ ExpectStart $ CmdAddBreakpoint (PRow 430) []
      , ExpectStatus [isWaiting] Nothing
      , ExpectResponse (Right (ResAddBreakpoint (UserBreakpoint 1 (PRow 430) [0]))) (Just (CmdAddBreakpoint (PRow 432) []))
      , ExpectResponse (Right (ResAddBreakpoint (UserBreakpoint 2 (PRow 432) [0]))) (Just (CmdAddBreakpoint (PRow 445) []))
      , ExpectResponse (Right (ResAddBreakpoint (UserBreakpoint 3 (PRow 445) [1]))) (Just CmdRun)
      , ExpectResponse (Right ResRun) Nothing
      , ExpectStatus [isRunning] Nothing
      , ExpectStatus [isStopped, threadStopped 0 1 430] (Just CmdContinue)
      , ExpectResponse (Right ResContinue) Nothing
      , ExpectStatus [isRunning] Nothing
      , ExpectStatus [isStopped, threadStopped 1 3 445] (Just CmdContinue)
      , ExpectResponse (Right ResContinue) Nothing
      , ExpectStatus [isRunning] Nothing
      , ExpectStatus [isStopped, threadStopped 0 2 432] (Just CmdShutdown)
      , ExpectResponse (Right ResShutdown) Nothing
      , ExpectStatus [isShutdown] Nothing
      ]
    -- "step" over critical function call with thread filter {{{3
    -- due to the thread filter breakpoint 2 is not hit (in contrast to previous test without thread filter)
    , [ ExpectStart $ CmdFilter [0]
      , ExpectStatus [isWaiting] Nothing
      , ExpectResponse (Right ResFilter) Nothing
      , ExpectStatus [isWaiting, withFilter [0]] (Just (CmdAddBreakpoint (PRow 430) []))
      , ExpectResponse (Right (ResAddBreakpoint (UserBreakpoint 1 (PRow 430) [0]))) (Just (CmdAddBreakpoint (PRow 432) []))
      , ExpectResponse (Right (ResAddBreakpoint (UserBreakpoint 2 (PRow 432) [0]))) (Just (CmdAddBreakpoint (PRow 445) []))
      , ExpectResponse (Right (ResAddBreakpoint (UserBreakpoint 3 (PRow 445) [1]))) (Just CmdRun)
      , ExpectResponse (Right ResRun) Nothing
      , ExpectStatus [isRunning] Nothing
      , ExpectStatus [isStopped, threadStopped 0 1 430] (Just CmdContinue)
      , ExpectResponse (Right ResContinue) Nothing
      , ExpectStatus [isRunning] Nothing
      , ExpectStatus [isStopped, threadStopped 0 2 432] (Just CmdShutdown)
      , ExpectResponse (Right ResShutdown) Nothing
      , ExpectStatus [isShutdown] Nothing
      ]
    -- print local variable of non-critical function
    , [ ExpectStart $ CmdAddBreakpoint (PRow 461) []
      , ExpectStatus [isWaiting] Nothing
      , ExpectResponse (Right (ResAddBreakpoint (UserBreakpoint 1 (PRow 461) [0, 1, 2]))) (Just CmdRun)
      , ExpectResponse (Right ResRun) Nothing
      , ExpectStatus [isRunning] Nothing
      , ExpectStatus [isStopped, threadStopped 2 1 461] (Just (CmdEvaluate "i"))
      , ExpectResponse (Right (ResEvaluate "0")) (Just CmdContinue)
      , ExpectResponse (Right ResContinue) Nothing
      , ExpectStatus [isRunning] Nothing
      , ExpectStatus [isStopped, threadStopped 2 1 461] (Just (CmdEvaluate "i"))
      , ExpectResponse (Right (ResEvaluate "0")) (Just CmdShutdown)
      , ExpectResponse (Right ResShutdown) Nothing
      , ExpectStatus [isShutdown] Nothing
      ]

    -- end {{{3
  ]
  where
    -- utils {{{3
    isRunning, isWaiting, isShutdown, isStopped :: Status -> Bool
    isRunning  = (ExRunning==) . statusExecution
    isWaiting  = (ExWaiting==) . statusExecution
    isShutdown = (ExShutdown==) . statusExecution
    isStopped  = (ExStopped==) . statusExecution

    threadStopped :: Int -> Int -> Int -> Status -> Bool
    threadStopped tid bid prow status =
      case find ((tid==) . thId) (statusThreads status) of
        Nothing -> False
        Just thread -> thProw thread == Just (PRow prow) && thStatus thread == Stopped (Just bid)

    withFilter :: [Int] -> Status -> Bool
    withFilter tids = (tids==) . statusThreadFilter

    runTest :: ExpectScript -> Assertion -- {{{3
    runTest (ExpectStart command:future) = do
      let options = Options "test/debug.json" "test/ec.elf" Nothing False 
      core <- setup options
      actor <- new_actor (0 :: Int, future)
      fCommand <- mfix(\fCommand' ->
          create_network
            core options
            (fire actor fCommand' (InputResponse . snd))
            (fire actor fCommand' InputStatus)
        )
      fCommand command
      result <- wait actor
      case result of
        Left e -> throwIO e
        Right _ -> return ()

    runTest _ = assertFailure "illegal script. Expect scripts must start with ExpectStart"

    fire actor fCommand mkInput x = update actor $ step actor fCommand (mkInput x)

    step _ _ input (count, []) = do
      assertFailure $ printf "%.2d: incomplete script. Next input: '%s'" count (show input)
      fail ""

    step actor fCommand input (count, (expected:rest)) = do
      putStrLn $ printf "%.2d: %s" count (show input)
      cmd <- handle input expected
      when (isJust cmd) (fCommand (fromJust cmd))
      when (null rest) (quit actor)
      return (count + 1, rest)

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

    handle (InputStatus status) (ExpectStatus fs cmd) = do
      (and . sequence fs) status @? "unexpected status: " ++ show status
      return cmd

    handle (InputResponse response) (ExpectResponse response' cmd) = do
      response' @=? response
      return cmd

    handle _ (ExpectStart _) = do
      assertFailure "illegal script. ExpectStart may only appear at the head of the expect script"
      fail ""

    handle input expect = do
      assertFailure $ "unexpected input: " ++ show expect ++ " / " ++ show input
      fail ""
