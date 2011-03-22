module Ocram.Output (writeAst) where

import Language.C.Syntax.AST (CTranslUnit)
import System.IO (openFile, IOMode(WriteMode), hClose, hPutStrLn)
import Language.C (pretty)

writeAst :: CTranslUnit -> String -> IO ()
writeAst ctu file = do
	outh <- openFile file WriteMode
	let code = show $ pretty ctu
	hPutStrLn outh code
	hPutStrLn outh main
	hClose outh

main = "int main() { return 0; }"
