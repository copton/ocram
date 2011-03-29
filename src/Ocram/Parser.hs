module Ocram.Parser (
	parse
) where

import Ocram.Types (Result, RawAst(RawAst))
import Ocram.Options (Options, optInput, optCppOptions)
import Language.C (parseCFile)
import Language.C.System.GCC (newGCC)

parse :: Options -> IO (Result RawAst)
parse options = do
	let cpp = "-DOCRAM_MODE" : words (optCppOptions options)
	let inp = optInput options
	result <- parseCFile (newGCC "gcc") Nothing cpp inp
	case result of
		Left e -> return $ fail $ "parsing failed\n" ++ show e
		Right ast -> return $ return $ RawAst ast
