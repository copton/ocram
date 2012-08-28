{-# LANGUAGE TemplateHaskell #-}
module Ocram.Options 
-- export {{{1
(
  Options(..), options, defaultOptions
) where

-- import {{{1
import Data.List (intercalate)
import Data.Maybe (isJust)
import Ocram.Text (new_error, OcramError)
import Ocram.Util (fromJust_s)
import Prelude hiding (abs)
import System.Console.GetOpt
import System.Path (absNormPath)

data Options = Options { -- {{{1
    optInput :: FilePath
  , optOutput :: FilePath
  , optPreprocessor :: FilePath
  , optPalGenerator :: Maybe FilePath
  , optPalFile :: Maybe FilePath
  , optDebugFile :: Maybe FilePath
  , optHelp :: Bool
}


options :: String -> FilePath -> [String] -> Either [OcramError] Options -- {{{1
options prg cwd argv =
  let use = usage prg in
  case getOpt Permute (availableOptions cwd) argv of
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
usage prg = usageInfo ("Usage: " ++ prg ++ " OPTIONS") (availableOptions "")

availableOptions :: FilePath -> [OptDescr (Options -> Options)]
availableOptions cwd = [
    Option "i" ["input"] (ReqArg (\x opts -> opts {optInput = abs x}) "input") "input tc file (required)"
  , Option "o" ["output"] (ReqArg (\x opts -> opts {optOutput = abs x}) "output") "output ec file (required)"
  , Option "c" ["preprocessor"] (ReqArg (\x opts -> opts {optPreprocessor = x}) "preprocessor") "external program which performs the pre-processing of the input C file (required)"
  , Option "g" ["generator"] (ReqArg (\x opts -> opts {optPalGenerator = Just x}) "generator") "external program which generates the PAL implementation (optional)"
  , Option "p" ["pal"] (ReqArg (\x opts -> opts {optPalFile = Just (abs x)}) "pal") "file path for the PAL generator program (mandatory if generator is specified)"
  , Option "d" ["debug"] (ReqArg (\x opts -> opts {optDebugFile = Just (abs x)}) "debug") "file path for the debugging information (optional)"
  , Option "h" ["help"] (NoArg (\opts -> opts {optHelp = True}))  "print help and quit"
  ]
    where abs = $fromJust_s . absNormPath cwd

defaultOptions :: Options
defaultOptions = Options { 
    optInput = ""
  , optOutput = ""
  , optPreprocessor = ""
  , optPalGenerator = Nothing
  , optPalFile = Nothing
  , optDebugFile = Nothing
  , optHelp = False
}

checkOptions :: Options -> Bool
checkOptions opts
  | optInput opts == optInput defaultOptions = False
  | optOutput opts == optOutput defaultOptions = False
  | isJust (optPalGenerator opts) && not (isJust (optPalFile opts)) = False
  | optPreprocessor opts == optPreprocessor defaultOptions = False
  | otherwise = True
