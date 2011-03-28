module Ocram.Output (writeAst) where

import Ocram.Types (AST, Result)
import Ocram.Options (Options, optOutput)
import Language.C.Syntax.AST (CTranslUnit)
import System.IO (openFile, IOMode(WriteMode), hClose, hPutStrLn)
import Language.C (pretty)

writeAst :: Options -> AST -> IO (Result ())
writeAst options ast = do
	let file = optOutput options
	outh <- openFile file WriteMode
	let code = show $ pretty ast 
	hPutStrLn outh code
	hPutStrLn outh main
	hClose outh
	return $ return ()

main = "int main() { return 0; }"
