module Ocram.Options (
	Options(..), getOptions
) where

import System.Environment (getArgs, getProgName)
import System.Console.GetOpt

data Options = Options { 
	  optInput :: String
	, optOutput :: String
	, optCppOptions :: String
	, optHelp :: Bool
} deriving Show

data Exit = Error [String] | Help [String]

instance Show Exit where
	show (Error ss) = unlines $ "Error in command line options" : ss
	show (Help ss) = unlines $ "Printing usage" : ss

options :: [OptDescr (Options -> Options)]
options = [
	  Option ['i'] ["input"] (ReqArg (\ x opts -> opts {optInput = x}) "input") "input tc file (required)"
	, Option ['o'] ["output"] (ReqArg (\ x opts -> opts {optOutput = x}) "output") "output ec file (required)"
	, Option ['c'] ["cpp"] (ReqArg (\ x opts -> opts {optCppOptions = x}) "cpp") "cpp options (default=\"\")"
	, Option ['h'] ["help"] (NoArg (\opts -> opts {optHelp = True}))  "print help and quit"
	]

defaultOptions = Options { 
	  optInput = ""
	, optOutput = ""
	, optCppOptions = ""
	, optHelp = False
}

checkOptions :: Options -> Bool
checkOptions opts
	| (optInput opts) == (optInput defaultOptions) = False
	| (optOutput opts) == (optOutput defaultOptions) = False
	| otherwise = True

usage :: String -> String
usage prg = usageInfo ("Usage: " ++ prg ++ " OPTIONS") options
	
getOptions :: IO (Either Exit Options)
getOptions = do
	argv <- getArgs
	prg <- getProgName
	let use = usage prg
	case getOpt Permute options argv of
		(o,[],[]) -> 
			let opts = foldl (flip id) defaultOptions o in
			if (optHelp opts) then
					return $ Left $ Help [use]
			else
				if checkOptions opts then
					return $ Right opts
				else
					return $ Left $ Error ["missing required option(s)", use]
		(_,n,[]) -> return $ Left $ Error ["unknown options '" ++ unwords n ++ "'", use]
		(_,_,e) -> return $ Left $ Error $ e ++ [use]
