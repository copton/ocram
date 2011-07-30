module Ocram.Options (
	Options(..), options, defaultOptions
) where

import Ocram.Types (EIO, Options(..))
import System.Environment (getArgs, getProgName)
import Data.List (intersperse)
import System.Console.GetOpt

import Control.Monad.Trans (liftIO)
import Control.Monad.Error (throwError)

options :: EIO Options
options = do
	argv <- liftIO getArgs
	prg <- liftIO getProgName
	let use = usage prg
	case getOpt Permute available_options argv of
		(o,[],[]) -> 
			let opts = foldl (flip id) defaultOptions o in
			if (optHelp opts) then
					throwError $ help use
			else
				if checkOptions opts then
					return opts
				else
					throwError $ err use "missing required option(s)"
		(_,n,[]) -> throwError $ err use ("unknown options '" ++ unwords n ++ "'")
		(_,_,es) -> throwError $ err use $ concat $ intersperse "\n" es

help :: String -> String
help use = "Printing usage:\n" ++ use

err :: String -> String -> String
err use msg = "Error in command line options\n" ++ msg ++ "\n" ++ use

usage :: String -> String
usage prg = usageInfo ("Usage: " ++ prg ++ " OPTIONS") available_options

available_options :: [OptDescr (Options -> Options)]
available_options = [
	  Option ['i'] ["input"] (ReqArg (\ x opts -> opts {optInput = x}) "input") "input tc file (required)"
	, Option ['o'] ["output"] (ReqArg (\ x opts -> opts {optOutput = x}) "output") "output ec file (required)"
	, Option ['c'] ["cpp"] (ReqArg (\ x opts -> opts {optCppOptions = x}) "cpp") "cpp options (default=\"\")"
	, Option ['s'] ["scheme"] (ReqArg (\x opts -> opts {optScheme = x }) "scheme") "compilation scheme (default=inline"
	, Option ['h'] ["help"] (NoArg (\opts -> opts {optHelp = True}))  "print help and quit"
	]

defaultOptions = Options { 
	  optInput = ""
	, optOutput = ""
	, optCppOptions = ""
	, optScheme = "inline"
	, optHelp = False
}

checkOptions :: Options -> Bool
checkOptions opts
	| (optInput opts) == (optInput defaultOptions) = False
	| (optOutput opts) == (optOutput defaultOptions) = False
	| otherwise = True

