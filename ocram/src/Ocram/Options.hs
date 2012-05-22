module Ocram.Options 
-- export {{{1
(
  Options(..), options, defaultOptions
) where

-- import {{{1
import Data.List (intercalate)
import Data.Maybe (isJust)
import Ocram.Text (new_error, OcramError)
import System.Console.GetOpt

-- types {{{1
data Options = Options {
    optInput :: String
  , optOutput :: String
  , optPalFile :: Maybe FilePath
  , optDebugFile :: Maybe FilePath
  , optPreprocessor :: FilePath
  , optPalGenerator :: Maybe FilePath
  , optHelp :: Bool
} deriving Show


-- options :: String -> [String] -> Either String Options {{{1
options :: String -> [String] -> Either [OcramError] Options
options prg argv =
  let use = usage prg in
  case getOpt Permute available_options argv of
    (o,[],[]) -> 
      let opts = foldl (flip id) defaultOptions o in
      if optHelp opts then
          Left [new_error 1 (help use) Nothing]
      else
        if checkOptions opts then
          Right opts
        else
          Left [new_error 2 (err use "missing required option(s)") Nothing]
    (_,n,[]) -> Left [new_error 3 (err use ("unknown options '" ++ unwords n ++ "'")) Nothing]
    (_,_,es) -> Left [new_error 4 (err use $ intercalate "\n" es) Nothing]

help :: String -> String
help use = "Printing usage:\n" ++ use

err :: String -> String -> String
err use msg = "Error in command line options\n" ++ msg ++ "\n" ++ use

usage :: String -> String
usage prg = usageInfo ("Usage: " ++ prg ++ " OPTIONS") available_options

available_options :: [OptDescr (Options -> Options)]
available_options = [
    Option "i" ["input"] (ReqArg (\x opts -> opts {optInput = x}) "input") "input tc file (required)"
  , Option "o" ["output"] (ReqArg (\x opts -> opts {optOutput = x}) "output") "output ec file (required)"
  , Option "p" ["pal"] (ReqArg (\x opts -> opts {optPalFile = Just x}) "pal") "target file path for the PAL generator program (mandatory if generator is specified)"
  , Option "d" ["debug"] (ReqArg (\x opts -> opts {optDebugFile = Just x}) "debug") "target file path for debug information (optional)"
  , Option "c" ["preprocessor"] (ReqArg (\x opts -> opts {optPreprocessor = x}) "preprocessor") "external program which performs the pre-processing of the input C file (required)"
  , Option "g" ["generator"] (ReqArg (\x opts -> opts {optPalGenerator = Just x}) "generator") "external program which generates the PAL implementation (optional)"
  , Option "h" ["help"] (NoArg (\opts -> opts {optHelp = True}))  "print help and quit"
  ]

defaultOptions :: Options
defaultOptions = Options { 
    optInput = ""
  , optOutput = ""
  , optPalFile = Nothing
  , optDebugFile = Nothing
  , optPalGenerator = Nothing
  , optPreprocessor = ""
  , optHelp = False
}

checkOptions :: Options -> Bool
checkOptions opts
  | optInput opts == optInput defaultOptions = False
  | optOutput opts == optOutput defaultOptions = False
  | isJust (optPalGenerator opts) && not (isJust (optPalFile opts)) = False
  | optPreprocessor opts == optPreprocessor defaultOptions = False
  | otherwise = True
