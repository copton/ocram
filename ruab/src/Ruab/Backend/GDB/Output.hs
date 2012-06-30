module Ruab.Backend.GDB.Output where

import qualified Data.Map as M
import qualified Ruab.Backend.GDB.Representation as R

-- types {{{1
data Response = Response (Maybe (ResponseType, Dictionary))

data ResponseType
  = Done
  | Running
  | Connected
  | Error
  | Exit

type Dictionary = M.Map String Value

data Value
  = ConstValue String
  | DictValue Dictionary
  | ListValue [Value]

data Notification = Notification NotifcationType Event Dictionary

data NotifcationType
  = Exec
  | Status
  | Notify

data Event
  = Stopped
  | ThreadGroupAdded

data Stream = Stream StreamType String

data StreamType
  = Console
  | Target
  | Log

-- conversion {{{1
response :: R.Output -> Response
response (R.Output _ Nothing) = Response Nothing
response (R.Output _ (Just (R.ResultRecord _ rc res))) = Response (Just (rt rc, dictionary res))
  where
    rt R.RCDone = Done
    rt R.RCRunning = Running
    rt R.RCConnected = Connected
    rt R.RCError = Error
    rt R.RCExit = Exit

notification :: R.Output -> [Notification]
notification (R.Output oobs _) = map (notification' . unp) $ filter isNotification oobs
  where
    isNotification (R.OOBAsyncRecord _) = True
    isNotification _ = False

    unp (R.OOBAsyncRecord x) = x

    notification' (R.ARExecAsyncOutput (R.ExecAsyncOutput _ (R.AsyncOutput ac res))) = Notification Exec (event ac) (dictionary res) 
    notification' (R.ARStatusAsyncOutput (R.StatusAsyncOutput _ (R.AsyncOutput ac res))) = Notification Status (event ac) (dictionary res)
    notification' (R.ARNotifyAsyncOutput (R.NotifyAsyncOutput _ (R.AsyncOutput ac res))) = Notification Notify (event ac) (dictionary res)

    event R.ACStop = Stopped
    event R.ACThreadGroupAdded = ThreadGroupAdded

stream :: R.Output -> [Stream]
stream (R.Output oobs _) = map (stream' . unp) $ filter isStream oobs
  where
    isStream (R.OOBStreamRecord _) = True
    isStream _ = False

    unp (R.OOBStreamRecord x) = x

    stream' (R.SRConsoleStreamOutput (R.ConsoleStreamOutput s)) = Stream Console s
    stream' (R.SRTargetStreamOutput (R.TargetStreamOutput s)) = Stream Target s
    stream' (R.SRLogStreamOutput (R.LogStreamOutput s)) = Stream Log s

dictionary :: [R.Result] -> Dictionary
dictionary res = M.fromList $ map entry res
  where
    entry (R.Result k v) = (k, value v)
    value (R.VConst s) = ConstValue s
    value (R.VTuple (R.Tuple res)) = DictValue (dictionary res)
    value (R.VList (R.EmptyList)) = ListValue []
    value (R.VList (R.ValueList vs)) = ListValue (map value vs)
    value (R.VList (R.ResultList res)) = DictValue (dictionary res)
