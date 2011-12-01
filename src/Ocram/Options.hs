module Ocram.Options 
-- export {{{1
(
  Options(..), options, defaultOptions, PalAction(..)
) where

-- import {{{1
import Data.List (intercalate)
import Ocram.Text (new_error, OcramError)
import System.Console.GetOpt

-- types {{{1
data PalAction = Compare | Dump deriving Show

data Options = Options { 
    optInput :: String
  , optOutput :: String
  , optCppOptions :: String
  , optPalHeader :: String
  , optPalAction :: PalAction
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
    Option "i" ["input"] (ReqArg (\ x opts -> opts {optInput = x}) "input") "input tc file (required)"
  , Option "o" ["output"] (ReqArg (\ x opts -> opts {optOutput = x}) "output") "output ec file (required)"
  , Option "c" ["cpp"] (ReqArg (\ x opts -> opts {optCppOptions = x}) "cpp") "cpp options (optional)"
  , Option "p" ["pal"] (ReqArg (\x opts -> opts {optPalHeader = x}) "pal") "PAL header file (optional)"
  , Option "d" ["dump"] (NoArg (\opts -> opts {optPalAction = Dump})) "Dump the PAL header instead of comparing it."
  , Option "h" ["help"] (NoArg (\opts -> opts {optHelp = True}))  "print help and quit"
  ]

defaultOptions :: Options
defaultOptions = Options { 
    optInput = ""
  , optOutput = ""
  , optCppOptions = ""
  , optPalHeader = ""
  , optPalAction = Compare
  , optHelp = False
}

checkOptions :: Options -> Bool
checkOptions opts
  | optInput opts == optInput defaultOptions = False
  | optOutput opts == optOutput defaultOptions = False
  | otherwise = True

