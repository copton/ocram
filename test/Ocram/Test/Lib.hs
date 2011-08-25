module Ocram.Test.Lib (
	parse, paste
) where

import Ocram.Types (Ast)
import Data.ByteString.Char8 (pack)
import Language.C.Data.Position (position)
import Language.C.Parser (parseC)

import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH (stringE)


parse :: String -> Ast
parse code = case parseC code' pos of
	Left e -> error $ show e
	Right ast -> ast
	where
		code' = pack code
		pos = position 0 "<<test>>" 0 0

paste = QuasiQuoter { 
	quoteExp = stringE, 
	quotePat = undefined,
	quoteType = undefined,
	quoteDec = undefined 
	}
