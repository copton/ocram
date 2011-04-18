module Ocram.Options (
	Options(..), getOptions, emptyOptions
) where

import Ocram.Types (Result, Options(..))
import System.Environment (getArgs, getProgName)
import Data.List (intersperse)
import System.Console.GetOpt

getOptions :: IO (Result Options)
getOptions = do
	argv <- getArgs
	prg <- getProgName
	let use = usage prg
	case getOpt Permute options argv of
		(o,[],[]) -> 
			let opts = foldl (flip id) emptyOptions o in
			if (optHelp opts) then
					return $ fail $ help use
			else
				if checkOptions opts then
					return $ return opts
				else
					return $ fail $ err use "missing required option(s)"
		(_,n,[]) -> return $ fail $ err use ("unknown options '" ++ unwords n ++ "'")
		(_,_,es) -> return $ fail $ err use $ concat $ intersperse "\n" es

help :: String -> String
help use = "Printing usage:\n" ++ use

err :: String -> String -> String
err use msg = "Error in command line options\n" ++ msg ++ "\n" ++ use

usage :: String -> String
usage prg = usageInfo ("Usage: " ++ prg ++ " OPTIONS") options

options :: [OptDescr (Options -> Options)]
options = [
	  Option ['i'] ["input"] (ReqArg (\ x opts -> opts {optInput = x}) "input") "input tc file (required)"
	, Option ['o'] ["output"] (ReqArg (\ x opts -> opts {optOutput = x}) "output") "output ec file (required)"
	, Option ['c'] ["cpp"] (ReqArg (\ x opts -> opts {optCppOptions = x}) "cpp") "cpp options (default=\"\")"
	, Option ['h'] ["help"] (NoArg (\opts -> opts {optHelp = True}))  "print help and quit"
	]

emptyOptions = Options { 
	  optInput = ""
	, optOutput = ""
	, optCppOptions = ""
	, optHelp = False
}

checkOptions :: Options -> Bool
checkOptions opts
	| (optInput opts) == (optInput emptyOptions) = False
	| (optOutput opts) == (optOutput emptyOptions) = False
	| otherwise = True

