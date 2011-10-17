module Ocram.Parser 
-- export {{{1
(
	parse
) where

-- import {{{1
import Control.Monad.Error (throwError)
import Control.Monad.Trans (liftIO)
import Language.C (parseCFile)
import Language.C.System.GCC (newGCC)
import Ocram.Options (Options, optInput, optCppOptions)
import Ocram.Text (OcramError, new_error)
import Ocram.Types (Ast)

-- parse :: Options -> IO (Either [OcramError] Ast) {{{1
parse :: Options -> IO (Either [OcramError] Ast)
parse options = do
	let cpp = "-DOCRAM_MODE" : words (optCppOptions options)
	let inp = optInput options
	result <- parseCFile (newGCC "gcc") Nothing cpp inp
	return $ case result of
		Left e -> Left [new_error 1 ("parsing failed\n" ++ show e) Nothing]
		Right ast -> Right ast
