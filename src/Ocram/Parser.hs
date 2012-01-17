module Ocram.Parser 
-- export {{{1
(
  parse
) where

-- import {{{1
import Language.C.Data.InputStream (readInputStream)
import Language.C.Data.Position (initPos)
import Language.C.Parser (parseC)
import Ocram.Options (Options)
import Ocram.Text (OcramError, new_error)
import Ocram.Types (Ast)

parse :: Options -> String -> IO (Either [OcramError] Ast) -- {{{1
parse _ input = do
  istream <- readInputStream input
  return $ case parseC istream (initPos input) of
    Left e -> Left [new_error 1 ("parsing failed\n" ++ show e) Nothing]
    Right ast -> Right ast
