module Ocram.Parser 
-- export {{{1
(
  parse
) where

-- import {{{1
import Language.C (parseCFile)
import Language.C.System.GCC (newGCC)
import Ocram.Options (Options, optCppOptions)
import Ocram.Text (OcramError, new_error)
import Ocram.Types (Ast)

parse :: Options -> String -> IO (Either [OcramError] Ast) -- {{{1
parse options input = do
  let cpp = "-DOCRAM_MODE" : words (optCppOptions options)
  result <- parseCFile (newGCC "gcc") Nothing cpp input
  return $ case result of
    Left e -> Left [new_error 1 ("parsing failed\n" ++ show e) Nothing]
    Right ast -> Right ast
