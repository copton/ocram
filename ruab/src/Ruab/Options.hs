module Ruab.Options
-- export {{{1
(
  Options(..), options
) where

-- imports {{{1
import Data.List (intercalate)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.Console.GetOpt (getOpt, usageInfo, ArgOrder(Permute), OptDescr, ArgDescr(NoArg, ReqArg), OptDescr(Option))

data Options = Options { -- {{{1
    optDebugFile :: FilePath
  , optHelp :: Bool
}

options :: String -> [String] -> Either (ExitCode, String) Options
options prg argv =
  case getOpt Permute availableOptions argv of
    (o, [], []) ->
      let opts = foldl (flip id) defaultOptions o in
      if optHelp opts
        then Left (ExitSuccess, help)
        else if checkOptions opts
          then Right opts
          else Left (ExitFailure 1, help)
    (_,n,[]) -> Left (ExitFailure 2, err ("unknown options '" ++ unwords n ++ "'"))
    (_,_,es) -> Left (ExitFailure 3, err (intercalate "\n" es))
  where
    help = "Printing usage:\n" ++ usage
    err msg = "Error in command line options\n" ++ msg ++ "\n" ++ usage
    usage = usageInfo ("Usage: " ++ prg ++ " OPTIONS") availableOptions
          
availableOptions :: [OptDescr (Options -> Options)]
availableOptions = [
    Option "d" ["debug"] (ReqArg (\x opts -> opts {optDebugFile = x}) "debug") "file path for the debugging information (required)"     
  , Option "h" ["help"] (NoArg (\opts -> opts {optHelp = True})) "print help and quit"
  ]

defaultOptions :: Options
defaultOptions = Options {
    optDebugFile = ""
  , optHelp = False
  }

checkOptions :: Options -> Bool
checkOptions opts
  | optDebugFile opts == optDebugFile defaultOptions = False
  | otherwise = True
