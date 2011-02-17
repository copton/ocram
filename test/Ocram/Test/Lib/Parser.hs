module Ocram.Test.Lib.Parser (
    parse
) where

import qualified Data.ByteString.Char8 as B
import Language.C.Syntax.AST (CTranslUnit)
import Language.C.Data.Position (position)
import Language.C.Parser (parseC)

errorOnLeft :: (Show a) => String -> (Either a b) -> b
errorOnLeft msg = either (error . ((msg ++ ": ")++).show) id

parse :: String -> CTranslUnit
parse code = errorOnLeft "parse error" $ parseC code' pos
    where
    code' = B.pack code
    pos = position 0 "<<test>>" 0 0
