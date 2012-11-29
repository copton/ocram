{-# LANGUAGE QuasiQuotes, FlexibleInstances, MultiParamTypeClasses, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Ruab.Core.Test (tests) where

-- imports {{{1
import Control.Exception.Base (try, SomeException)
import Control.Monad.Fix (mfix)
import Control.Monad (when)
import Data.List (intercalate, find)
import Data.Maybe (fromJust, isJust)
import Ruab.Actor (new_actor, update, quit, monitor)
import Ruab.Core
import Ruab.Options (Options(..))
import Ruab.Test.Lib (enumTestGroup)
import Test.Framework (Test, testGroup)
import Test.HUnit (Assertion, (@?), (@=?), assertFailure)
import Text.Printf (printf)

tests :: Test -- {{{1
tests = testGroup "Core" [test_integration]

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
     -- 01 - start and quit {{{3
      [ ExpectStart CmdRun
      , ExpectStatus [isWaiting] Nothing
      , ExpectResponse (Right ResRun) (Just CmdShutdown)
      , ExpectStatus [isRunning] Nothing
      , ExpectResponse (Right ResShutdown) Nothing
      , ExpectStatus [isShutdown] Nothing
      ]
     -- 02 - break point in critical function {{{3
    , [ ExpectStart $ CmdAddBreakpoint (PRow 515) []
      , ExpectStatus [isWaiting] Nothing
      , ExpectResponse (Right (ResAddBreakpoint (UserBreakpoint 1 (PRow 515) [0, 1]))) (Just CmdRun)
      , ExpectResponse (Right ResRun) Nothing
      , ExpectStatus [isRunning] Nothing
      , ExpectStatus [isStopped, threadStopped 0 (Just 1) 515] (Just CmdShutdown)
      , ExpectResponse (Right ResShutdown) Nothing
      , ExpectStatus [isShutdown] Nothing
      ]
     -- 03 - break point with condition in critical function {{{3
    , [ ExpectStart $ CmdAddBreakpoint (PRow 515) [1]
      , ExpectStatus [isWaiting] Nothing
      , ExpectResponse (Right (ResAddBreakpoint (UserBreakpoint 1 (PRow 515) [1]))) (Just CmdRun)
      , ExpectResponse (Right ResRun) Nothing
      , ExpectStatus [isRunning] Nothing
      , ExpectStatus [isStopped, threadStopped 1 (Just 1) 515] (Just CmdShutdown)
      , ExpectResponse (Right ResShutdown) Nothing
      , ExpectStatus [isShutdown] Nothing
      ]
    -- 04 - break point in non-critical function {{{3
    , [ ExpectStart $ CmdAddBreakpoint (PRow 461) []
      , ExpectStatus [isWaiting] Nothing
      , ExpectResponse (Right (ResAddBreakpoint (UserBreakpoint 1 (PRow 461) [0, 1, 2]))) (Just CmdRun)
      , ExpectResponse (Right ResRun) Nothing
      , ExpectStatus [isRunning] Nothing
      , ExpectStatus [isStopped, threadStopped 2 (Just 1) 461] (Just CmdShutdown)
      , ExpectResponse (Right ResShutdown) Nothing
      , ExpectStatus [isShutdown] Nothing
      ]
    -- 05 - "step" over critical function call {{{3
    , [ ExpectStart $ CmdAddBreakpoint (PRow 430) []
      , ExpectStatus [isWaiting] Nothing
      , ExpectResponse (Right (ResAddBreakpoint (UserBreakpoint 1 (PRow 430) [0]))) (Just (CmdAddBreakpoint (PRow 432) []))
      , ExpectResponse (Right (ResAddBreakpoint (UserBreakpoint 2 (PRow 432) [0]))) (Just (CmdAddBreakpoint (PRow 445) []))
      , ExpectResponse (Right (ResAddBreakpoint (UserBreakpoint 3 (PRow 445) [1]))) (Just CmdRun)
      , ExpectResponse (Right ResRun) Nothing
      , ExpectStatus [isRunning] Nothing
      , ExpectStatus [isStopped, threadStopped 0 (Just 1) 430] (Just CmdContinue)
      , ExpectResponse (Right ResContinue) Nothing
      , ExpectStatus [isRunning] Nothing
      , ExpectStatus [isStopped, threadStopped 1 (Just 3) 445] (Just CmdContinue)
      , ExpectResponse (Right ResContinue) Nothing
      , ExpectStatus [isRunning] Nothing
      , ExpectStatus [isStopped, threadStopped 0 (Just 2) 432] (Just CmdShutdown)
      , ExpectResponse (Right ResShutdown) Nothing
      , ExpectStatus [isShutdown] Nothing
      ]
    -- 06 - "step" over critical function call with thread filter {{{3
    -- due to the thread filter breakpoint 3 is not hit (in contrast to previous test without thread filter)
    , [ ExpectStart $ CmdFilter [0]
      , ExpectStatus [isWaiting] Nothing
      , ExpectResponse (Right ResFilter) Nothing
      , ExpectStatus [isWaiting, withFilter [0]] (Just (CmdAddBreakpoint (PRow 430) []))
      , ExpectResponse (Right (ResAddBreakpoint (UserBreakpoint 1 (PRow 430) [0]))) (Just (CmdAddBreakpoint (PRow 432) []))
      , ExpectResponse (Right (ResAddBreakpoint (UserBreakpoint 2 (PRow 432) [0]))) (Just (CmdAddBreakpoint (PRow 445) []))
      , ExpectResponse (Right (ResAddBreakpoint (UserBreakpoint 3 (PRow 445) [1]))) (Just CmdRun)
      , ExpectResponse (Right ResRun) Nothing
      , ExpectStatus [isRunning] Nothing
      , ExpectStatus [isStopped, threadStopped 0 (Just 1) 430] (Just CmdContinue)
      , ExpectResponse (Right ResContinue) Nothing
      , ExpectStatus [isRunning] Nothing
      , ExpectStatus [isStopped, threadStopped 0 (Just 2) 432] (Just CmdShutdown)
      , ExpectResponse (Right ResShutdown) Nothing
      , ExpectStatus [isShutdown] Nothing
      ]
    -- 07 - print local variable of non-critical function {{{3
    , [ ExpectStart $ CmdAddBreakpoint (PRow 461) []
      , ExpectStatus [isWaiting] Nothing
      , ExpectResponse (Right (ResAddBreakpoint (UserBreakpoint 1 (PRow 461) [0, 1, 2]))) (Just CmdRun)
      , ExpectResponse (Right ResRun) Nothing
      , ExpectStatus [isRunning] Nothing
      , ExpectStatus [isStopped, threadStopped 2 (Just 1) 461] (Just (CmdEvaluate "i"))
      , ExpectResponse (Right (ResEvaluate "0")) (Just CmdContinue)
      , ExpectResponse (Right ResContinue) Nothing
      , ExpectStatus [isRunning] Nothing
      , ExpectStatus [isStopped, threadStopped 2 (Just 1) 461] (Just (CmdEvaluate "i"))
      , ExpectResponse (Right (ResEvaluate "0")) (Just CmdShutdown)
      , ExpectResponse (Right ResShutdown) Nothing
      , ExpectStatus [isShutdown] Nothing
      ]
    -- 08 - print non-existent local variable of non-critical function {{{3
    , [ ExpectStart $ CmdAddBreakpoint (PRow 461) []
      , ExpectStatus [isWaiting] Nothing
      , ExpectResponse (Right (ResAddBreakpoint (UserBreakpoint 1 (PRow 461) [0, 1, 2]))) (Just CmdRun)
      , ExpectResponse (Right ResRun) Nothing
      , ExpectStatus [isRunning] Nothing
      , ExpectStatus [isStopped, threadStopped 2 (Just 1) 461] (Just (CmdEvaluate "j"))
      , ExpectResponse (Left "No symbol \"j\" in current context.") (Just CmdShutdown)
      , ExpectResponse (Right ResShutdown) Nothing
      , ExpectStatus [isShutdown] Nothing
      ]
    -- 09 - print local variable of critical function {{{3
    , [ ExpectStart $ CmdAddBreakpoint (PRow 431) []
      , ExpectStatus [isWaiting] Nothing
      , ExpectResponse (Right (ResAddBreakpoint (UserBreakpoint 1 (PRow 431) [0]))) (Just CmdRun)
      , ExpectResponse (Right ResRun) Nothing
      , ExpectStatus [isRunning] Nothing
      , ExpectStatus [isStopped, threadStopped 0 (Just 1) 431] (Just (CmdEvaluate "now"))
      , ExpectResponse (Right (ResEvaluate "0")) (Just CmdContinue)
      , ExpectResponse (Right ResContinue) Nothing
      , ExpectStatus [isRunning] Nothing
      , ExpectStatus [isStopped, threadStopped 0 (Just 1) 431] (Just (CmdEvaluate "now"))
      , ExpectResponse (Right (ResEvaluate "50")) (Just CmdShutdown)
      , ExpectResponse (Right ResShutdown) Nothing
      , ExpectStatus [isShutdown] Nothing
      ]
    -- 10 - step in non-critical function {{{3
    , [ExpectStart $ CmdAddBreakpoint (PRow 459) []
      , ExpectStatus [isWaiting] Nothing
      , ExpectResponse (Right (ResAddBreakpoint (UserBreakpoint 1 (PRow 459) [0, 1, 2]))) (Just CmdRun)
      , ExpectResponse (Right ResRun) Nothing
      , ExpectStatus [isRunning] Nothing
      , ExpectStatus [isStopped, threadStopped 2 (Just 1) 459] (Just CmdContinue)
      , ExpectResponse (Right ResContinue) Nothing
      , ExpectStatus [isRunning] Nothing
      , ExpectStatus [isStopped, threadStopped 2 (Just 1) 459] (Just (CmdStepNext Next))
      , ExpectResponse (Right ResStepNext) Nothing
      , ExpectStatus [isRunning] Nothing
      , ExpectStatus [isStopped, threadStopped 2 Nothing 461] (Just CmdShutdown)
      , ExpectResponse (Right ResShutdown) Nothing
      , ExpectStatus [isShutdown] Nothing
    ]
    -- 11 - step through with filter {{{3
    , [ExpectStart $ CmdFilter [2]
      , ExpectStatus [isWaiting] Nothing
      , ExpectResponse (Right ResFilter) Nothing
      , ExpectStatus [isWaiting, withFilter [2]] (Just (CmdAddBreakpoint (PRow 532) []))
      , ExpectResponse (Right (ResAddBreakpoint (UserBreakpoint 1 (PRow 532) [2]))) (Just CmdRun)
      , ExpectResponse (Right ResRun) Nothing
      , ExpectStatus [isRunning] Nothing
      , ExpectStatus [isStopped, threadStopped 2 (Just 1) 532] (Just (CmdStepNext Step))
    ] ++ stepByStep Step 2 [
        494
      , 494
      , 495
      , 495
      , 496
      , 496
      , 497
      , 497
      , 500
      , 501
      , 503
      , 504
      , 506
      , 472
      , 473
      , 474
      , 475
      , 477
      , 477
      , 478
      , 480
      , 459
      , 481
      , 507
      , 472
      , 473
      , 474
      , 475
      , 477
      , 477
      , 478
      , 480
      , 459
      , 481
      , 509
      , 500
    ] ++ [
        ExpectResponse (Right ResShutdown) Nothing
      , ExpectStatus [isShutdown] Nothing
    ]
    -- 12 - next through with filter {{{3
{-
    , [ExpectStart $ CmdFilter [2]
      , ExpectStatus [isWaiting] Nothing
      , ExpectResponse (Right ResFilter) Nothing
      , ExpectStatus [isWaiting, withFilter [2]] (Just (CmdAddBreakpoint (PRow 532) []))
      , ExpectResponse (Right (ResAddBreakpoint (UserBreakpoint 1 (PRow 532) [2]))) (Just CmdRun)
      , ExpectResponse (Right ResRun) Nothing
      , ExpectStatus [isRunning] Nothing
      , ExpectStatus [isStopped, threadStopped 2 (Just 1) 532] (Just (CmdStepNext Next))
    ] ++ stepByStep Next 2 [
        494
      , 495
      , 496
      , 497
      , 500
      , 501
      , 503
      , 504
      , 506
      , 507
      , 509
      , 500
    ] ++ [
        ExpectResponse (Right ResShutdown) Nothing
      , ExpectStatus [isShutdown] Nothing
    ] 
-}
  -- end {{{3
  ]
  where
    -- utils {{{3
    stepByStep :: StepNextType -> Int -> [Int] -> [Expect]
    stepByStep snt thread = reverse . concat . zipWith go (True:repeat False) . reverse
      where
        go isLast row = reverse [
            ExpectResponse (Right ResStepNext) Nothing
          , ExpectStatus [isRunning] Nothing
          , ExpectStatus [isStopped, threadStopped thread Nothing row] (Just (if isLast then CmdShutdown else CmdStepNext snt))
          ]

    isRunning, isWaiting, isShutdown, isStopped :: Status -> Bool
    isRunning  = exRunning . statusExecution
      where exRunning (ExRunning _) = True
            exRunning _             = False
    isWaiting  = (ExWaiting==) . statusExecution
    isShutdown = (ExShutdown==) . statusExecution
    isStopped  = (ExStopped==) . statusExecution

    threadStopped :: Int -> Maybe Int -> Int -> Status -> Bool
    threadStopped tid mbid prow status =
      case find ((tid==) . thId) (statusThreads status) of
        Nothing -> False
        Just thread -> thProw thread == Just (PRow prow) && thStatus thread == Stopped mbid

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
      monitor actor

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
