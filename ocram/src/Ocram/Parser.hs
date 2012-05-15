module Ocram.Parser 
-- export {{{1
(
  parse
) where

-- import {{{1
import Language.C.Syntax.AST (CTranslUnit)
import Language.C.Data.Position (initPos)
import Language.C.Parser (parseC)
import Ocram.Options (Options)
import Ocram.Text (OcramError, new_error)

import qualified Data.ByteString.Char8 as BS

parse :: Options -> String -> IO (Either [OcramError] (BS.ByteString, CTranslUnit)) -- {{{1
parse _ input = do
  tcode <- BS.readFile input
  return $ case parseC tcode (initPos input) of
    Left e -> Left [new_error 1 ("parsing failed\n" ++ show e) Nothing]
    Right ast -> Right (tcode, ast)
