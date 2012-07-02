module Ruab.Backend.GDB.Responses where

import Control.Applicative ((<$>), (<*>))
import Ruab.Backend.GDB.Output (Dictionary, Value(..))

import qualified Data.Map as Map

-- types {{{1
data Breakpoint = Breakpoint { -- {{{2
    bpktNumber           :: Int
  , bpktType             :: BreakpointType
  , bpktDisp             :: BreakpointDisp
  , bkptEnabled          :: Bool
  , bkptAddress          :: String
  , bkptFunc             :: String
  , bkptFile             :: String
  , bkptFullname         :: String
  , bkptLine             :: Int
  , bkptTimes            :: Int
  , bkptOriginalLocation :: String
  }

type BreakpointType = String

data BreakpointDisp
  = BreakpointKeep
  | BreakpointDel

instance Read BreakpointDisp where
  readsPrec _ "del" = [(BreakpointDel, "")]
  readsPrec _ "keep" = [(BreakpointKeep, "")]
  readsPrec _ _ = []

-- responses {{{1
response_break_insert :: Dictionary -> Maybe Breakpoint -- {{{2
response_break_insert dict = do
  value <- Map.lookup "bkpt" dict
  dict' <- asDict value
  Breakpoint
    <$> get dict' tryRead "number"
    <*> get dict' Just    "type" 
    <*> get dict' tryRead "disp"
    <*> get dict' gdbBool "enabled"
    <*> get dict' Just    "addr"
    <*> get dict' Just    "func"
    <*> get dict' Just    "file"
    <*> get dict' Just    "fullname"
    <*> get dict' tryRead "line"
    <*> get dict' tryRead "times"
    <*> get dict' Just    "original-location"

-- utils {{{1
get :: Dictionary -> (String -> Maybe a) -> (String -> Maybe a) -- {{{2
get dict parse key = Map.lookup key dict >>= asConst >>= parse

asDict :: Value -> Maybe Dictionary -- {{{2
asDict (DictValue d) = Just d
asDict _ = Nothing

asConst :: Value -> Maybe String -- {{{2
asConst (ConstValue s) = Just s
asConst _ = Nothing

asList :: Value -> Maybe [Value] -- {{{2
asList (ListValue vs) = Just vs
asList _ = Nothing

tryRead :: Read a => String -> Maybe a -- {{{2
tryRead str = case readsPrec 0 str of
  [(x, "")] -> Just x
  _ -> Nothing

gdbBool :: String -> Maybe Bool
gdbBool "y" = Just True
gdbBool "n" = Just False
gdbBool _ = Nothing
