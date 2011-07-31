module Ocram.Test.Lib (
	parse, paste
) where

import Ocram.Types
import Ocram.Options (defaultOptions, Options)
import qualified Data.ByteString.Char8 as B
import Language.C.Data.Position (position)
import Language.C.Parser (parseC)

import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH (stringL, litE, litP)


parse :: String -> Ast
parse code = case parseC code' pos of
	Left e -> error $ show e
	Right ast -> ast
	where
		code' = B.pack code
		pos = position 0 "<<test>>" 0 0

paste = QuasiQuoter (litE . stringL) (litP . stringL) 
