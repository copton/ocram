module Main where

import System.Environment (getArgs, getProgName)
import Monad (when)

import Language.C
import Language.C.System.GCC

main = do
    args <- getArgs
    when (length args < 1) usage
    let file = head args
    ast <- parseMyFile file
    printNodeInfo ast
--    printMyAst ast
    where 
        usage = getProgName >>= \pn -> error $ "usage: " ++ pn ++ " <file>"
      
parseMyFile :: FilePath -> IO CTranslUnit
parseMyFile input_file =
  do parse_result <- parseCFile (newGCC "gcc") Nothing [] input_file
     case parse_result of
       Left parse_err -> error (show parse_err)
       Right ast      -> return ast

printNodeInfo :: CTranslUnit -> IO [()]
printNodeInfo (CTranslUnit decls ni) = sequence $ pn ni : map pd decls
    where pn (OnlyPos pos _) = putStrLn $ show pos
          pn (NodeInfo pos _ name) = putStrLn $ show name ++ ": " ++ show pos
          pd (CDeclExt (CDecl _ _ ni)) = putStrLn $ "CDeclExt: " ++ show ni
          pd (CFDefExt (CFunDef _ _ _ _ ni)) = putStrLn $ "CFDefExt: " ++ show ni
          pd (CAsmExt (CStrLit _ ni) _) = putStrLn $ "CAsmExt: " ++ show ni

printMyAst :: CTranslUnit -> IO ()
printMyAst ctu = (print . pretty) ctu
