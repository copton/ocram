module Ocram.Options (
	Options(..), getOptions
) where

import System.Environment (getArgs, getProgName)
import System.Console.GetOpt

data Options = Options { 
	  optApiHeader :: String
	, optApplication :: String
	, optCppOptions :: String
	, optHelp :: Bool
} deriving Show

data Exit = Error [String] | Help [String]

instance Show Exit where
	show (Error ss) = unlines $ "Error in command line options" : ss
	show (Help ss) = unlines $ "Printing usage" : ss

options :: [OptDescr (Options -> Options)]
options = [
	  Option ['a'] ["api"] (ReqArg (\ x opts -> opts { optApiHeader = x }) "api") "api header file (required)"
	, Option ['i'] ["input"] (ReqArg (\ x opts -> opts {optApplication = x}) "input") "input c file (required)"
	, Option ['c'] ["cpp"] (ReqArg (\ x opts -> opts {optCppOptions = x}) "cpp") "cpp options (default=\"\")"
	, Option ['h'] ["help"] (NoArg (\opts -> opts {optHelp = True}))  "print help and quit"
	]

defaultOptions = Options { 
	  optApiHeader = ""
	, optApplication = ""
	, optCppOptions = ""
	, optHelp = False
}

checkOptions :: Options -> Bool
checkOptions opts
	| (optApiHeader opts) == (optApiHeader defaultOptions) = False
	| (optApplication opts) == (optApplication defaultOptions) = False
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
