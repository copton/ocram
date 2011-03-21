module Ocram.Options (
	Options(..), getOptions
) where

import System.Environment (getArgs, getProgName)
import System.Console.GetOpt

data Options = Options { 
	  optApiHeader :: String
	, optApplication :: String
	, optCppOptions :: String
} deriving Show

newtype Error = Error [String]

instance Show Error where
	show (Error ss) = unlines ss

options :: [OptDescr (Options -> Options)]
options = [
	  Option ['a'] ["api"] (ReqArg (\ x opts -> opts { optApiHeader = x }) "api") "api header file (required)"
	, Option ['i'] ["input"] (ReqArg (\ x opts -> opts {optApplication = x}) "input") "input c file (required)"
	, Option ['c'] ["cpp"] (ReqArg (\ x opts -> opts {optCppOptions = x}) "cpp") "cpp options (default=\"\")"
	]

defaultOptions = Options { 
	  optApiHeader = ""
	, optApplication = ""
	, optCppOptions = ""
}

checkOptions :: Options -> Bool
checkOptions opts
	| (optApiHeader opts) == (optApiHeader defaultOptions) = False
	| (optApplication opts) == (optApplication defaultOptions) = False
	| otherwise = True

usage :: String -> String
usage prg = usageInfo ("Usage: " ++ prg ++ " OPTIONS") options
	
getOptions :: IO (Either Error Options)
getOptions = do
	argv <- getArgs
	prg <- getProgName
	case getOpt Permute options argv of
		(o,[],[]) -> 
			let opts = foldl (flip id) defaultOptions o in
			if checkOptions opts then
				return $ Right opts
			else
				return $ Left $ Error ["missing required option(s)", usage prg]
		(_,n,[]) -> return $ Left $ Error ["unknown options '" ++ unwords n ++ "'", (usage prg)]
		(_,_,e) -> return $ Left $ Error $ e ++ [usage prg]
