module Ocram.Output (writeAst) where

import Ocram.Types (OutputAst, Result, getAst)
import Ocram.Options (Options, optOutput)
import Language.C.Syntax.AST (CTranslUnit)
import System.IO (openFile, IOMode(WriteMode), hClose, hPutStrLn)
import Language.C (pretty)

writeAst :: Options -> OutputAst -> IO (Result ())
writeAst options output_ast = do
	let file = optOutput options
	outh <- openFile file WriteMode
	let code = show $ pretty $ getAst output_ast 
	hPutStrLn outh code
	hPutStrLn outh main
	hClose outh
	return $ return ()

main = "int main() { return 0; }"
