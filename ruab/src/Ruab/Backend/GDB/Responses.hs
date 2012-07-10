module Ruab.Backend.GDB.Responses where

-- import {{{1
import Control.Applicative ((<$>), (<*>))
import Control.Monad (forM, guard)
import Ruab.Backend.GDB.Output (Items, Value(..))

-- types {{{1
data Breakpoint = Breakpoint { -- {{{2
    bkptNumber           :: Int
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

type BreakpointType = String

data BreakpointDisp
  = BreakpointKeep
  | BreakpointDel
  deriving Show

instance Read BreakpointDisp where
  readsPrec _ "del" = [(BreakpointDel, "")]
  readsPrec _ "keep" = [(BreakpointKeep, "")]
  readsPrec _ _ = []

newtype Stack -- {{{2
  = Stack {stackFrames :: [Frame] }
  deriving (Show, Eq)

data Frame = Frame {
    frameLevel    :: Int
  , frameAddr     :: String
  , frameFunc     :: String
  , frameFile     :: String
  , frameFullname :: Maybe String
  , frameLine     :: Int
  } deriving (Show, Eq)

-- responses {{{1
response_break_insert :: Items -> Maybe Breakpoint -- {{{2
response_break_insert items = do
  value <- lookup "bkpt" items
  items' <- asItems value
  Breakpoint
    <$> get items' tryRead "number"
    <*> get items' Just    "type" 
    <*> get items' tryRead "disp"
    <*> get items' gdbBool "enabled"
    <*> get items' Just    "addr"
    <*> get items' Just    "func"
    <*> get items' Just    "file"
    <*> get items' Just    "fullname"
    <*> get items' tryRead "line"
    <*> get items' tryRead "times"
    <*> get items' Just    "original-location"

response_stack_list_frames :: Items -> Maybe Stack -- {{{2
response_stack_list_frames items = do
  value  <- lookup "stack" items
  values <- asItems value
  frames <- forM values (\(name, value') -> do
      guard (name == "frame")
      items' <- asItems value'
      Frame
        <$> get items' tryRead "level"
        <*> get items' Just "addr"
        <*> get items' Just "func"
        <*> get items' Just "file"
        <*> Just (get items' Just "fullname")
        <*> get items' tryRead "line"
    )
  return (Stack frames)

-- utils {{{1
get :: Items -> (String -> Maybe a) -> (String -> Maybe a) -- {{{2
get items parse key = lookup key items >>= asConst >>= parse

asItems :: Value -> Maybe Items -- {{{2
asItems (ItemsValue d) = Just d
asItems _ = Nothing

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
