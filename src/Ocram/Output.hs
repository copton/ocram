module Ocram.Output 
-- export {{{1
(
	pretty_print,
	writeDebugSymbols
) where

-- import {{{1
import Ocram.Types (Ast, DebugSymbols)
import Ocram.Options (Options, optOutput)
import Language.C.Syntax.AST (CTranslUnit)
import System.IO (openFile, IOMode(WriteMode), hClose, hPutStrLn)
import Language.C (pretty)
import Control.Monad.Trans (liftIO)
import Control.Monad.Error(throwError)

-- pretty_print :: Options -> Ast -> IO () {{{1
pretty_print :: Options -> Ast -> IO ()
pretty_print options ast = do
	let file = optOutput options
	let code = show $ pretty ast 
	let main = "int main() { return 0; }"
	write file $ code ++ "\n" ++ main

-- writeDebugSymbols :: Options -> DebugSymbols -> IO () {{{1
writeDebugSymbols :: Options -> DebugSymbols -> IO ()
writeDebugSymbols = undefined

-- utils {{{1
write :: String -> String -> IO ()
write filename contents = do
		outh <- openFile filename WriteMode
		hPutStrLn outh contents
		hClose outh
		return ()

