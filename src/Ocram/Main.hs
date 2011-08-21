module Ocram.Main (main) where

import Ocram.Types
import Ocram.Parser (parse)
import Ocram.Options (options)
import Ocram.Analysis (analysis)
import qualified Ocram.Transformation.Inline as Inline
import Ocram.Output (pretty_print, writeDebugSymbols)
import System.Exit (exitWith, ExitCode(ExitFailure))
import System.Environment (getArgs, getProgName)
import System.IO (stderr, hPutStrLn)

import Control.Monad.Error (runErrorT)

main = do
	argv <- getArgs	
	prg <- getProgName
	opt <- exitOnError $ options prg argv	
	ast <- exitOnError =<< parse opt
	ana <- exitOnError $ analysis ast
	trans <- exitOnError $ select_transformation $ optScheme opt
	let (ast', ds) = trans ana ast
	pretty_print opt ast'
	writeDebugSymbols opt ds
	return ()

select_transformation :: String -> Either String Transformation
select_transformation "inline" = Right Inline.transformation
select_transformation s = Left $ "unknown compilation scheme \"" ++ s ++ "\"."
	
exitOnError :: Either String a -> IO a
exitOnError (Left err) = hPutStrLn stderr err >> exitWith (ExitFailure 1)
exitOnError (Right x) = return x
