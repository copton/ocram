module Ocram.Main (main) where

import Ocram.Types
import Ocram.Parser (parse)
import Ocram.Options (options)
import Ocram.Compiler (analysis, transformation)
import Ocram.Output (pretty_print, writeDebugSymbols)

import System.Exit (exitWith, ExitCode(ExitFailure))
import System.IO (stderr, hPutStrLn)

import Control.Monad.Error (runErrorT)
	
main = do
	err <- runErrorT $ do
		opt <- options
		ast <- parse opt
		(ana, ast') <- analysis opt ast
		(ast'', ds) <- transformation opt ana ast'
		pretty_print opt ast''
		writeDebugSymbols opt ds
	report err

report :: Either String () -> IO ()
report (Left err) = hPutStrLn stderr err >> exitWith (ExitFailure 1)
report _ = return ()
