module Main where

import System.Environment (getArgs, getProgName)
import Monad (when)

import Language.C
import Language.C.System.GCC

main = do
    args <- getArgs
    when (length args < 1) usage
    let file = head args
    parseMyFile file >>= printMyAst
    where 
        usage = getProgName >>= \pn -> error $ "usage: " ++ pn ++ " <file>"
      
parseMyFile :: FilePath -> IO CTranslUnit
parseMyFile input_file =
  do parse_result <- parseCFile (newGCC "gcc") Nothing [] input_file
     case parse_result of
       Left parse_err -> error (show parse_err)
       Right ast      -> return ast

printMyAst :: CTranslUnit -> IO ()
printMyAst ctu = (print . pretty) ctu
