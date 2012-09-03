module Ruab.Backend.GDB.Responses where

-- import {{{1
import Control.Applicative ((<$>), (<*>))
import Control.Monad (guard, msum, (<=<))
import Data.List (find)
import Ruab.Backend.GDB.Representation

-- types {{{1
type BkptNumber = Int
data Breakpoint = Breakpoint { -- {{{2
    bkptNumber           :: BkptNumber
  , bkptType             :: BreakpointType
  , bkptDisp             :: BreakpointDisp
  , bkptEnabled          :: Bool
  , bkptAddress          :: String
  , bkptFunc             :: String
  , bkptFile             :: String
  , bkptFullname         :: String
  , bkptLine             :: Int
  , bkptTimes            :: Int
  , bkptOriginalLocation :: String
  }
  deriving Show

type BreakpointType = String -- {{{2

data BreakpointDisp -- {{{2
  = BreakpointKeep
  | BreakpointDel
  deriving Show

instance Read BreakpointDisp where
  readsPrec _ "del" = [(BreakpointDel, "")]
  readsPrec _ "keep" = [(BreakpointKeep, "")]
  readsPrec _ _ = []

newtype Stack -- {{{2
  = Stack {stackFrames :: [Frame] }
  deriving Show

data Frame = Frame { -- {{{2
    frameLevel    :: Maybe Int
  , frameAddr     :: String
  , frameFunc     :: String
  , frameArgs     :: Maybe [Arg]
  , frameFile     :: String
  , frameFullname :: Maybe String
  , frameLine     :: Int
  } deriving Show

data Stopped = Stopped { -- {{{2
      stoppedReason   :: StopReason
    , stoppedFrame    :: Frame
    , stoppedThreadId :: Int
    , stoppedThreads  :: String
    , stoppedCore     :: Int
  }
  deriving Show

data StopReason -- {{{2
  = BreakpointHit {
      bkptHitDisp   :: BreakpointDisp
    , bkptHitNumber :: BkptNumber
    }
  | EndSteppingRange
  deriving Show

data Arg = Arg { -- {{{2
    argName  :: String
  , argValue :: String
  } deriving Show

-- composition {{{1
responseBreakpoint :: Result -> Maybe Breakpoint -- {{{2
responseBreakpoint (Result variable value) = do
  guard (variable == "bkpt")
  (Tuple rs) <- asTuple value
  Breakpoint
    <$> get rs tryRead "number"
    <*> get rs Just    "type" 
    <*> get rs tryRead "disp"
    <*> get rs gdbBool "enabled"
    <*> get rs Just    "addr"
    <*> get rs Just    "func"
    <*> get rs Just    "file"
    <*> get rs Just    "fullname"
    <*> get rs tryRead "line"
    <*> get rs tryRead "times"
    <*> get rs Just    "original-location"

responseStack :: Result -> Maybe Stack -- {{{2
responseStack (Result variable value) = do
  guard (variable == "stack")
  list <- asList value
  case list of
    EmptyList -> Just $ Stack []
    ResultList is ->
      Stack <$> mapM responseFrame is
    _ -> Nothing

responseFrame :: Result -> Maybe Frame -- {{{2
responseFrame (Result variable value) = do
  guard (variable == "frame")
  (Tuple rs) <- asTuple value
  Frame
    <$> Just (get rs tryRead "level")
    <*>       get rs Just    "addr"
    <*>       get rs Just    "func"
    <*> Just (msum (map responseArgs rs))
    <*>       get rs Just   "file"
    <*> Just (get rs Just   "fullname")
    <*>       get rs tryRead "line"

responseStopped :: [Result] -> Maybe Stopped -- {{{2
responseStopped rs = do
  Stopped
    <$> responseStopReason rs
    <*> msum (map responseFrame rs)
    <*> get rs tryRead "thread-id"
    <*> get rs Just    "stopped-threads"
    <*> get rs tryRead "core"

responseStopReason :: [Result] -> Maybe StopReason  -- {{{2
responseStopReason rs = do
  reason <- find (("reason"==) . resVariable) rs >>= asConst . resValue
  case reason of 
    "breakpoint-hit" ->
      BreakpointHit
        <$> get rs tryRead "disp"
        <*> get rs tryRead "bkptno"
    "end-stepping-range" -> Just EndSteppingRange
    _ -> Nothing 

responseArgs :: Result -> Maybe [Arg] -- {{{2
responseArgs (Result variable value) = do
  guard (variable == "args")
  list <- asList value
  case list of
    EmptyList -> Just []
    ValueList is -> do
      mapM ((responseArg . tupleResults) <=< asTuple) is
    _ -> Nothing 

responseArg :: [Result] -> Maybe Arg -- {{{2
responseArg rs = do
  Arg
    <$> get rs Just "name"
    <*> get rs Just "value"

-- responses {{{1
response_stack_list_frames :: [Result] -> Maybe Stack -- {{{2
response_stack_list_frames [item] = responseStack item
response_stack_list_frames _      = Nothing

response_break_insert :: [Result] -> Maybe Breakpoint -- {{{2
response_break_insert [item] = responseBreakpoint item
response_break_insert _      = Nothing

response_data_evaluate_expression :: [Result] -> Maybe String -- {{{2
response_data_evaluate_expression [(Result variable value)] = do
  guard  (variable == "value")
  asConst value

response_data_evaluate_expression _ = Nothing -- {{{2

response_error :: [Result] -> Maybe String -- {{{2
response_error [(Result variable value)] = do
  guard (variable == "msg")
  asConst value
response_error _ = Nothing

response_stopped :: [Result] -> Maybe Stopped -- {{{2
response_stopped items = responseStopped items
  
-- utils {{{1
get :: [Result] -> (String -> Maybe a) -> (String -> Maybe a) -- {{{2
get rs parse key = find ((key==) . resVariable) rs >>= asConst . resValue >>= parse

tryRead :: Read a => String -> Maybe a -- {{{2
tryRead str = case readsPrec 0 str of
  [(x, "")] -> Just x
  _ -> Nothing

gdbBool :: String -> Maybe Bool -- {{{2
gdbBool "y" = Just True
gdbBool "n" = Just False
gdbBool _ = Nothing
