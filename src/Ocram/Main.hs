module Ocram.Main (main) where

import Control.Monad.Error
import Ocram.Types (Result, AST)
import Ocram.Options (getOptions, Options)
import Ocram.Output (writeAst)
import Ocram.Sanity (checkSanity)
import Ocram.Parser (parse)
import Ocram.Analysis (determineBlockingFunctions, getFunctions, findStartRoutines, determineCallGraph, determineCriticalFunctions, checkConstraints)
import Ocram.Transformation (tc2ec)

import System.Exit (exitWith, ExitCode(ExitFailure))
import System.IO (stderr, hPutStrLn)

process :: IO (Result ())
process = runErrorT $ do
	options <- ErrorT $ getOptions
	raw_ast <- ErrorT $ parse options
	output_ast <- ErrorT $ return $ process' options raw_ast
	result <- ErrorT $ writeAst options output_ast
	return result

process' :: Options -> AST -> Result AST
process' options raw_ast = do
	input_ast <- checkSanity raw_ast
	blocking_functions <- determineBlockingFunctions input_ast
	function_map <- getFunctions input_ast
	start_routines <- findStartRoutines function_map
	call_graph <- determineCallGraph input_ast function_map blocking_functions
	critical_functions <- determineCriticalFunctions call_graph function_map blocking_functions
	input_ast' <- checkConstraints input_ast critical_functions
	output_ast <- tc2ec input_ast'
	return output_ast

main :: IO ()
main = do
	result <- process
	case result of
		Left e -> do
			hPutStrLn stderr e
			exitWith (ExitFailure 1)
		Right _ -> return ()
