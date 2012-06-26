{-# LANGUAGE TemplateHaskell #-}
module Ruab.Backend.Representation where

-- [The GDB/MI Interface](http://sourceware.org/gdb/current/onlinedocs/gdb/GDB_002fMI.html#GDB_002fMI)
-- GDB version 7.4

-- whenever an MI command results in an error, we recommend that the frontend refreshes all the information shown in the user interface.
-- it is suggested to just always pass the `--thread' and `--frame' options
--

import Control.Applicative ((<$>), (<*>), (<*))
import Data.Char (isAscii)
import Data.Maybe (isNothing)
import Data.List (find)
import Ruab.Util (abort)

import Debug.Trace (trace)
import Text.ParserCombinators.Parsec

-- input {{{1
-- types {{{2
type Token = Integer -- {{{3

data Command -- {{{3
  = CLICommand (Maybe Token) String
  | MICommand (Maybe Token) Operation [Option] [Parameter]

type Operation = String -- {{{3

data Option = Option Parameter (Maybe Parameter) -- {{{3

type Parameter = String -- {{{3

-- rendering {{{2
r_command :: Command -> ShowS -- {{{3
r_command (CLICommand tok str) = maybe id r_token tok . showString str
r_command (MICommand tok operation options parameters) =
    maybe id shows tok
  . showString "-" . r_operation operation
  . foldl (\f o -> f . showString " " . r_option o) id options
  . if null parameters
    then id
    else showString " --" . foldl (\f p -> f . showString " " . r_parameter p) id parameters

r_operation :: Operation -> ShowS -- {{{3
r_operation op = (op++)

r_option :: Option -> ShowS -- {{{3
r_option (Option p p') =
    showString "-" . r_parameter p
  . maybe id (\x -> showString " " . r_parameter x) p'

r_parameter :: Parameter -> ShowS -- {{{3
r_parameter = showString

r_token :: Token -> ShowS -- {{{3
r_token = shows

-- output {{{1
-- types {{{2
data Output -- {{{3
  = Output [OutOfBandRecord] (Maybe ResultRecord)
  deriving Show

data ResultRecord -- {{{3
  = ResultRecord (Maybe Token) ResultClass [Result]
  deriving Show

data OutOfBandRecord -- {{{3
  = OOBAsyncRecord AsyncRecord
  | OOBStreamRecord StreamRecord
  deriving Show

data AsyncRecord -- {{{3
  = ARExecAsyncOutput ExecAsyncOutput
  | ARStatusAsyncOutput StatusAsyncOutput
  | ARNotifyAsyncOutput NotifyAsyncOutput
  deriving Show

data ExecAsyncOutput -- {{{3
  = ExecAsyncOutput (Maybe Token) AsyncOutput
  deriving Show

data StatusAsyncOutput -- {{{3
  = StatusAsyncOutput (Maybe Token) AsyncOutput
  deriving Show

data NotifyAsyncOutput -- {{{3
  = NotifyAsyncOutput (Maybe Token) AsyncOutput
  deriving Show

data AsyncOutput -- {{{3
  = AsyncOutput AsyncClass [Result]
  deriving Show

data ResultClass -- {{{3
  = RCDone
  | RCRunning
  | RCConnected
  | RCError
  | RCExit
  deriving Show
 
data AsyncClass -- {{{3
  = ACStop
  | ACThreadGroupAdded
  deriving Show

data Result -- {{{3
  = Result Variable Value
  deriving Show

type Variable = String -- {{{3

data Value -- {{{3
  = VConst Const
  | VTuple Tuple
  | VList  List
  deriving Show

type Const = CString -- {{{3

data Tuple -- {{{3
  = Tuple [Result]
  deriving Show

data List -- {{{3
  = EmptyList
  | ValueList [Value]
  | ResultList [Result]
  deriving Show

data StreamRecord  -- {{{3
  = SRConsoleStreamOutput ConsoleStreamOutput
  | SRTargetStreamOutput TargetStreamOutput
  | SRLogStreamOutput LogStreamOutput
  deriving Show

data ConsoleStreamOutput -- {{{3
  = ConsoleStreamOutput CString
  deriving Show

data TargetStreamOutput -- {{{3
  = TargetStreamOutput CString
  deriving Show

data LogStreamOutput -- {{{3
  = LogStreamOutput CString
  deriving Show

type CString = String -- {{{3

-- parsing {{{2
p_output :: Parser Output -- {{{3
p_output = Output <$> many p_outOfBandRecord <*> optionMaybe p_resultRecord <* string "(gdb)" <* newline <* eof

p_resultRecord :: Parser ResultRecord -- {{{3
p_resultRecord =
  ResultRecord <$> optionMaybe p_token <* char '^' <*> p_resultClass <*> many (char ',' >> p_result) <* newline

p_outOfBandRecord :: Parser OutOfBandRecord -- {{{3
p_outOfBandRecord =
      (p_asyncRecord  >>= return . OOBAsyncRecord)
  <|> (p_streamRecord >>= return . OOBStreamRecord)

p_asyncRecord :: Parser AsyncRecord -- {{{3
p_asyncRecord =
      (p_execAsyncOutput   >>= return . ARExecAsyncOutput)
  <|> (p_statusAsyncOutput >>= return . ARStatusAsyncOutput)
  <|> (p_notifyAsyncOutput >>= return . ARNotifyAsyncOutput)

p_execAsyncOutput :: Parser ExecAsyncOutput -- {{{3
p_execAsyncOutput =
  ExecAsyncOutput <$> optionMaybe p_token <* char '*' <*> p_asyncOutput

p_statusAsyncOutput :: Parser StatusAsyncOutput -- {{{3
p_statusAsyncOutput =
  StatusAsyncOutput <$> optionMaybe p_token <* char '+' <*> p_asyncOutput

p_notifyAsyncOutput :: Parser NotifyAsyncOutput -- {{{3
p_notifyAsyncOutput =
  NotifyAsyncOutput <$> optionMaybe p_token <* char '=' <*> p_asyncOutput

p_asyncOutput :: Parser AsyncOutput -- {{{3
p_asyncOutput =
  AsyncOutput <$> p_asyncClass <*> many (char ',' >> p_result) <* newline

p_resultClass :: Parser ResultClass -- {{{3
p_resultClass =
      (string "done"      >> return RCDone)
  <|> (string "running"   >> return RCRunning)
  <|> (string "connected" >> return RCConnected)
  <|> (string "error"     >> return RCError)
  <|> (string "exit"      >> return RCExit)

p_asyncClass :: Parser AsyncClass -- {{{3
p_asyncClass =
      (string "stopped" >> return ACStop)
  <|> (string "thread-group-added" >> return ACThreadGroupAdded)

p_result :: Parser Result -- {{{3
p_result =
  Result <$> p_variable <* char '=' <*> p_value

p_variable :: Parser Variable -- {{{3
p_variable = many1 (letter <|> digit <|> oneOf "_")

p_value :: Parser Value -- {{{3
p_value =
      (p_const >>= return . VConst)
  <|> (p_tuple >>= return . VTuple)
  <|> (p_list  >>= return . VList)

p_const :: Parser Const -- {{{3
p_const = p_cString

p_tuple :: Parser Tuple -- {{{3
p_tuple = try p_emptyTuple <|> p_filledTuple
  where
    p_emptyTuple = string "{}" >> return (Tuple [])
    p_filledTuple = do
      _ <- char '{'
      first <- p_result
      rest <- many (char ',' >> p_result)
      _ <- char '}'
      return $ Tuple (first:rest)

p_list :: Parser List -- {{{3
p_list = try p_emptyList <|> try p_valueList <|> p_resultList
  where
    p_emptyList = string "[]" >> return EmptyList
    p_valueList = do
      _ <- char '['
      first <- p_value
      rest <- many (char ',' >> p_value)
      _ <- char ']'
      return $ ValueList (first:rest)
  
    p_resultList = do
      _ <- char '['
      first <- p_result
      rest <- many (char ',' >> p_result)
      _ <- char ']'
      return $ ResultList (first:rest)

p_streamRecord :: Parser StreamRecord -- {{{3
p_streamRecord = do
  sr <- anyStreamRecord
  _ <- newline
  return sr
  where
    anyStreamRecord =
          (p_consoleStreamOutput >>= return . SRConsoleStreamOutput)
      <|> (p_targetStreamOutput  >>= return . SRTargetStreamOutput)
      <|> (p_logStreamOutput     >>= return . SRLogStreamOutput)

p_consoleStreamOutput :: Parser ConsoleStreamOutput -- {{{3
p_consoleStreamOutput = char '~' >> p_cString >>= return . ConsoleStreamOutput

p_targetStreamOutput :: Parser TargetStreamOutput -- {{{3
p_targetStreamOutput = char '@' >> p_cString >>= return . TargetStreamOutput

p_logStreamOutput :: Parser LogStreamOutput -- {{{3
p_logStreamOutput = char '&' >> p_cString >>= return . LogStreamOutput

p_cString :: Parser CString -- {{{3
p_cString = between (char '"') (char '"') (many p_cchar)
  where
    p_cchar = p_cbackslash
          <|> noneOf "\""
    p_cbackslash = do
      _ <- char '\\'
      c <- anyChar
      case c of
        '\\' -> return '\\'
        'n' -> return '\n'
        '"' -> return '"'
        _ -> fail $ "unknown backslash escape: " ++ show c

p_token :: Parser Token -- {{{3
p_token = many1 digit >>= (\x -> trace (show x) ((return . read) x))

-- utils {{{1
render_input :: Command -> String
render_input cmd = r_command cmd ""

parse_output :: String -> Output
parse_output str = case parse p_output "gdb" str of
  Left pe -> $abort $ "parse failed: " ++ show pe
  Right o -> o

parameterValid :: Parameter -> Bool
parameterValid param
  | null param = False
  | isCString param = isNothing $ find (not . isAscii) param
  | otherwise = isNothing $ find isSpecial param
  where
    isCString ('"':rest) = last rest == '"'
    isCString _ = False

    isSpecial ' ' = True
    isSpecial '-' = True
    isSpecial '\n' = True
    isSpecial '"' = True
    isSpecial _ = False

