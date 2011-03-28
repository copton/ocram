module Ocram.Test.Lib (
	parse, getAst
) where

import Ocram.Types (Result, AST)
import qualified Data.ByteString.Char8 as B
import Language.C.Data.Position (position)
import Language.C.Parser (parseC)

getAst :: String -> AST
getAst code = case parse code of
	Left l -> error l
	Right r -> r

parse :: String -> Result AST
parse code = case parseC code' pos of
	Left e -> fail $ show e
	Right ast -> return ast
	where
		code' = B.pack code
		pos = position 0 "<<test>>" 0 0
