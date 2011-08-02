module Ocram.Parser 
-- export {{{1
(
	parse
) where

-- import {{{1
import Ocram.Types (EIO, Ast)
import Ocram.Options (Options, optInput, optCppOptions)
import Language.C (parseCFile)
import Language.C.System.GCC (newGCC)
import Control.Monad.Error (throwError)
import Control.Monad.Trans (liftIO)

-- parse :: Options -> EIO Ast {{{1
parse :: Options -> EIO Ast
parse options = do
	let cpp = "-DOCRAM_MODE" : words (optCppOptions options)
	let inp = optInput options
	result <- liftIO $ parseCFile (newGCC "gcc") Nothing cpp inp
	case result of
		Left e -> throwError $ "parsing failed\n" ++ show e
		Right ast -> return ast
