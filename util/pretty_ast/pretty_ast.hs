import Prelude hiding (print)
import Language.Haskell.Pretty (prettyPrint)
import Language.Haskell.Parser (ParseResult(ParseOk, ParseFailed), parseModule)

import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(ExitFailure))
import System.IO (hPutStrLn, stderr)
import Control.Monad (when)
import Text.PrettyPrint.HughesPJ (render, text, (<+>), hsep)

import Language.C              -- simple API
import Language.C.System.GCC   -- preprocessor used

usageMsg :: String -> String
usageMsg prg = render $ text "Usage:" <+> text prg <+> hsep (map text ["CPP_OPTIONS","input_file.c"])

print (ParseOk hsm) = prettyPrint hsm
print (ParseFailed loc msg) = error $ show loc ++ ": " ++ msg

decorate ast = "foo " ++ show ast ++ " = undefined"

clean str = map replace str
	where
		replace c
			| c == '`' = '"'
			| c == '\'' = '"'
			| otherwise = c

main :: IO ()
main = do
	let usageErr = (hPutStrLn stderr (usageMsg "./ParseAndPrint") >> exitWith (ExitFailure 1))
	args <- getArgs
	when (length args < 1) usageErr
	let (opts,input_file) = (init args, last args)

	ast <- errorOnLeftM "Parse Error" $ parseCFile (newGCC "gcc") Nothing opts input_file
	putStrLn $ print $ parseModule $ clean $ decorate ast
	return ()

errorOnLeft :: (Show a) => String -> (Either a b) -> IO b
errorOnLeft msg = either (error . ((msg ++ ": ")++).show) return
errorOnLeftM :: (Show a) => String -> IO (Either a b) -> IO b
errorOnLeftM msg action = action >>= errorOnLeft msg
