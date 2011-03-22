module Ocram.Main (main) where

import Language.C (parseCFile)
import Language.C.System.GCC (newGCC)
import Language.C.Syntax.AST (CTranslUnit)

import Ocram.CreateContext (createContext)
import Ocram.Context (Context(ctxNewAst))
import Ocram.Options (getOptions, Options(..))
import Ocram.Output (writeAst)

main :: IO ()
main = do
	options <- errorOnLeftM Nothing $ getOptions
	let cpp = "-DOCRAM_MODE" : words (optCppOptions options)
	let inp = optInput options
	let oup = optOutput options
	ast <- errorOnLeftM (Just "Error when parsing input file") (parseCFile (newGCC "gcc") Nothing cpp inp)
	let ctx = createContext ast
	writeAst (ctxNewAst ctx) oup

errorOnLeftM :: (Show a) => Maybe String -> IO (Either a b) -> IO b
errorOnLeftM msg action = action >>= errorOnLeft msg

errorOnLeft :: (Show a) => Maybe String -> (Either a b) -> IO b
errorOnLeft (Just msg) = either (error . ((msg ++ "\n")++).show) return
errorOnLeft Nothing = either (error . show) return
