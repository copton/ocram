module Ocram.Main (main) where

import Control.Monad.Error
import Ocram.Types (Result, RawAst, OutputAst, Options, Context(getOutputAst))
import Ocram.Options (getOptions)
import Ocram.Parser (parse)
import Ocram.Context (context)
import Ocram.Output (writeAst)

import System.Exit (exitWith, ExitCode(ExitFailure))
import System.IO (stderr, hPutStrLn)

process :: IO (Result ())
process = runErrorT $ do
	options <- ErrorT $ getOptions
	raw_ast <- ErrorT $ parse options
	let ctx = context options raw_ast
	output_ast <- ErrorT $ return $ getOutputAst ctx
	result <- ErrorT $ writeAst options output_ast
	return result

main :: IO ()
main = do
	result <- process
	case result of
		Left e -> do
			hPutStrLn stderr e
			exitWith (ExitFailure 1)
		Right _ -> return ()
