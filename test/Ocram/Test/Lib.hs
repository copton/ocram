module Ocram.Test.Lib (
	parse, parse', createContext
) where

import Ocram.Types (Result, RawAst(RawAst), Context(Context))
import Ocram.Options (emptyOptions, Options)
import Ocram.Context (context)
import qualified Data.ByteString.Char8 as B
import Language.C.Data.Position (position)
import Language.C.Parser (parseC)

parse' :: String -> RawAst
parse' code = case parse code of
	Left l -> error l
	Right r -> r

parse :: String -> Result RawAst
parse code = case parseC code' pos of
	Left e -> fail $ show e
	Right ast -> return (RawAst ast)
	where
		code' = B.pack code
		pos = position 0 "<<test>>" 0 0

createContext :: String -> Maybe Options -> Context
createContext code Nothing = context emptyOptions $ parse' code
createContext code (Just options) = context options $ parse' code
