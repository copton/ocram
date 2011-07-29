module Ocram.Output (
	pretty_print,
	writeDebugSymbols
) where

import Ocram.Types (Ast, DebugSymbols, EIO)
import Ocram.Options (Options, optOutput)
import Language.C.Syntax.AST (CTranslUnit)
import System.IO (openFile, IOMode(WriteMode), hClose, hPutStrLn)
import Language.C (pretty)
import Control.Monad.Trans (liftIO)
import Control.Monad.Error(throwError)

pretty_print :: Options -> Ast -> EIO ()
pretty_print options ast = do
	let file = optOutput options
	let code = show $ pretty ast 
	liftIO $ write file $ code ++ "\n" ++ main
	return ()

write :: String -> String -> IO ()
write filename contents = do
		outh <- openFile filename WriteMode
		hPutStrLn outh contents
		hClose outh
		return ()

writeDebugSymbols :: Options -> DebugSymbols -> EIO ()
writeDebugSymbols = undefined

main = "int main() { return 0; }"
