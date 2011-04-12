module Ocram.Main (main) where

import Control.Monad.Error
import Ocram.Types (Result, RawAst, OutputAst)
import Ocram.Options (getOptions, Options)
import Ocram.Parser (parse)
import Ocram.Analysis (determineBlockingFunctions, getFunctions, findStartRoutines, determineCallGraph, determineCriticalFunctions)
import Ocram.Filter (checkSanity, checkConstraints, checkRecursion)
import Ocram.Transformation (transformControlFlow, transformDataFlow)
import Ocram.Output (writeAst)

import System.Exit (exitWith, ExitCode(ExitFailure))
import System.IO (stderr, hPutStrLn)

process :: IO (Result ())
process = runErrorT $ do
	options <- ErrorT $ getOptions
	raw_ast <- ErrorT $ parse options
	output_ast <- ErrorT $ return $ process' options raw_ast
	result <- ErrorT $ writeAst options output_ast
	return result

process' :: Options -> RawAst -> Result OutputAst
process' options raw_ast = do
	sane_ast <- checkSanity raw_ast
	blocking_functions <- determineBlockingFunctions sane_ast
	function_map <- getFunctions sane_ast
	start_routines <- findStartRoutines function_map
	call_graph <- determineCallGraph sane_ast function_map blocking_functions
	cyclefree_ast <- checkRecursion sane_ast call_graph start_routines function_map
	critical_functions <- determineCriticalFunctions cyclefree_ast call_graph function_map blocking_functions
	valid_ast <- checkConstraints critical_functions cyclefree_ast
	stackless_ast <- transformDataFlow valid_ast critical_functions function_map
	output_ast <- transformControlFlow stackless_ast critical_functions function_map
	return output_ast

main :: IO ()
main = do
	result <- process
	case result of
		Left e -> do
			hPutStrLn stderr e
			exitWith (ExitFailure 1)
		Right _ -> return ()
