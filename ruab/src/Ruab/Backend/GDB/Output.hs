{-# LANGUAGE TemplateHaskell #-}
module Ruab.Backend.GDB.Output
-- exports {{{1
(
    Response (..) , ResponseType(..)
  , Dictionary
  , Value(..)
  , Notification(..), NotifcationType(..), Event(..)
  , Stream(..), StreamType(..)
  , has_result, result_is, dictionary
  , output_response, output_stream, output_notification
) where

-- imports {{{1
import Ruab.Util (abort)

import qualified Data.Map as M
import qualified Ruab.Backend.GDB.Representation as R

-- types {{{1
data Response -- {{{2
  = Response (Maybe (ResponseType, Dictionary))
  deriving Show

data ResponseType
  = Done
  | Running
  | Connected
  | Error
  | Exit
  deriving (Show, Eq)

type Dictionary = M.Map String Value -- {{{2

data Value -- {{{2
  = ConstValue String
  | DictValue Dictionary
  | ListValue [Value]
  deriving Show

data Notification -- {{{2
  = Notification NotifcationType Event Dictionary
  deriving Show

data NotifcationType
  = Exec
  | Status
  | Notify
  deriving Show

data Event
  = Stopped
  | ThreadGroupAdded
  | ThreadGroupStarted
  | ThreadCreated
  | ERunning
  | LibraryLoaded
  deriving Show

data Stream -- {{{2
  = Stream StreamType String
  deriving Show

data StreamType
  = Console
  | Target
  | Log
  deriving Show

-- helper {{{1
has_result :: Response -> Bool
has_result (Response Nothing) = False
has_result _ = True

result_is :: ResponseType -> Response -> Bool
result_is _ (Response Nothing) = False
result_is t (Response (Just (t', _))) = t == t'

dictionary :: Response -> Maybe Dictionary -- {{{2
dictionary (Response Nothing) = Nothing
dictionary (Response (Just (_, d))) = Just d

-- conversion {{{1
output_response :: R.Output -> Response
output_response (R.Output _ (Just (R.ResultRecord _ rc res))) = Response (Just (rt rc, outputDictionary res))
  where
    rt R.RCDone = Done
    rt R.RCRunning = Running
    rt R.RCConnected = Connected
    rt R.RCError = Error
    rt R.RCExit = Exit
output_response _ = Response Nothing

output_notification :: R.Output -> [Notification]
output_notification (R.Output oobs _) = map (notification' . unp) $ filter isNotification oobs
  where
    isNotification (R.OOBAsyncRecord _) = True
    isNotification _ = False

    unp (R.OOBAsyncRecord x) = x
    unp x = $abort $ "unexpected parameter: " ++ show x

    notification' (R.ARExecAsyncOutput (R.ExecAsyncOutput _ (R.AsyncOutput ac res))) = Notification Exec (event ac) (outputDictionary res) 
    notification' (R.ARStatusAsyncOutput (R.StatusAsyncOutput _ (R.AsyncOutput ac res))) = Notification Status (event ac) (outputDictionary res)
    notification' (R.ARNotifyAsyncOutput (R.NotifyAsyncOutput _ (R.AsyncOutput ac res))) = Notification Notify (event ac) (outputDictionary res)

    event R.ACStop = Stopped
    event R.ACThreadGroupAdded = ThreadGroupAdded
    event R.ACThreadGroupStarted = ThreadGroupStarted
    event R.ACThreadCreated = ThreadCreated
    event R.ACRunning = ERunning
    event R.ACLibraryLoaded = LibraryLoaded

output_stream :: R.Output -> [Stream]
output_stream (R.Output oobs _) = map (stream' . unp) $ filter isStream oobs
  where
    isStream (R.OOBStreamRecord _) = True
    isStream _ = False

    unp (R.OOBStreamRecord x) = x
    unp x = $abort $ "unexpected parameter: " ++ show x

    stream' (R.SRConsoleStreamOutput (R.ConsoleStreamOutput s)) = Stream Console s
    stream' (R.SRTargetStreamOutput (R.TargetStreamOutput s)) = Stream Target s
    stream' (R.SRLogStreamOutput (R.LogStreamOutput s)) = Stream Log s

outputDictionary :: [R.Result] -> Dictionary
outputDictionary res = M.fromList $ map entry res
  where
    entry (R.Result k v) = (k, value v)
    value (R.VConst s) = ConstValue s
    value (R.VTuple (R.Tuple res')) = DictValue (outputDictionary res')
    value (R.VList (R.EmptyList)) = ListValue []
    value (R.VList (R.ValueList vs)) = ListValue (map value vs)
    value (R.VList (R.ResultList res')) = DictValue (outputDictionary res')
