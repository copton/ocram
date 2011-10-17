module Ocram.Main (main) where

import Ocram.Types
import Ocram.Parser (parse)
import Ocram.Options (options, Options(optScheme))
import Ocram.Analysis (analysis, CallGraph)
import Ocram.Text (OcramError, show_errors, new_error)
--import qualified Ocram.Transformation.Inline as Inline
import Ocram.Output (pretty_print, writeDebugSymbols)
import System.Exit (exitWith, ExitCode(ExitFailure))
import System.Environment (getArgs, getProgName)
import System.IO (stderr, hPutStrLn)

import Control.Monad.Error (runErrorT)

main = do
	argv <- getArgs	
	prg <- getProgName
	opt <- exitOnError "options" $ options prg argv	
	ast <- exitOnError "parser" =<< parse opt
	ana <- exitOnError "analysis" $ analysis ast
	trans <- exitOnError "options" $ select_transformation $ optScheme opt
	let (ast', ds) = trans ana ast
	pretty_print opt ast'
	writeDebugSymbols opt ds
	return ()

type Transformation = CallGraph -> Ast -> (Ast, DebugSymbols)

select_transformation :: String -> Either [OcramError] Transformation
--select_transformation "inline" = Right Inline.transformation
select_transformation s = Left [new_error 1 ("unknown compilation scheme \"" ++ s ++ "\".") Nothing]

exitOnError :: String -> Either [OcramError] a -> IO a
exitOnError module_ (Left es) = hPutStrLn stderr (show_errors module_ es) >> exitWith (ExitFailure 1)
exitOnError _ (Right x) = return x
