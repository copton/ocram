module Ocram.Parser 
-- export {{{1
(
	parse
) where

-- import {{{1
import Ocram.Types (Ast)
import Ocram.Options (Options, optInput, optCppOptions)
import Language.C (parseCFile)
import Language.C.System.GCC (newGCC)
import Control.Monad.Error (throwError)
import Control.Monad.Trans (liftIO)

-- parse :: Options -> IO (Either String Ast) {{{1
parse :: Options -> IO (Either String Ast)
parse options = do
	let cpp = "-DOCRAM_MODE" : words (optCppOptions options)
	let inp = optInput options
	result <- parseCFile (newGCC "gcc") Nothing cpp inp
	return $ case result of
		Left e -> Left $ "parsing failed\n" ++ show e
		Right ast -> Right ast
