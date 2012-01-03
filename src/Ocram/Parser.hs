module Ocram.Parser 
-- export {{{1
(
  parse
) where

-- import {{{1
import Language.C (parseCFile)
import Language.C.System.GCC (newGCC)
import Ocram.Options (Options, optCppOptions, optToolchain)
import Ocram.Text (OcramError, new_error)
import Ocram.Types (Ast)

parse :: Options -> String -> IO (Either [OcramError] Ast) -- {{{1
parse options input = do
  let opts = "-DOCRAM_MODE" : words (optCppOptions options)
  let cpp = newGCC $ optToolchain options ++ "gcc"
  result <- parseCFile cpp Nothing opts input
  return $ case result of
    Left e -> Left [new_error 1 ("parsing failed\n" ++ show e) Nothing]
    Right ast -> Right ast
