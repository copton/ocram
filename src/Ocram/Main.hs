module Ocram.Main (main) where

import Language.C (parseCFile, pretty)
import Language.C.System.GCC (newGCC)
import Language.C.Syntax.AST (CTranslUnit)

import Ocram.CreateContext (createContext)
import Ocram.Options (getOptions, Options(..))

main :: IO ()
main = do
	options <- errorOnLeftM Nothing $ getOptions
	let cpp = words (optCppOptions options)
	let app = optApplication options
	ast <- errorOnLeftM (Just "Error when parsing input file") (parseCFile (newGCC "gcc") Nothing cpp app)
	let ctx = createContext ast
	printMyAST ast

errorOnLeft :: (Show a) => Maybe String -> (Either a b) -> IO b
errorOnLeft (Just msg) = either (error . ((msg ++ "\n")++).show) return
errorOnLeft Nothing = either (error . show) return
errorOnLeftM :: (Show a) => Maybe String -> IO (Either a b) -> IO b
errorOnLeftM msg action = action >>= errorOnLeft msg

printMyAST :: CTranslUnit -> IO ()
printMyAST ctu = (print . pretty) ctu
