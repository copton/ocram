module Ocram.Main (main) where

import Ocram.Analysis (analysis, CallGraph)
import Ocram.Options (options, Options(optScheme))
import Ocram.Output (pretty_print, write_debug_symbols)
import Ocram.Parser (parse)
import Ocram.Text (OcramError, show_errors, new_error)
import Ocram.Types (DebugSymbols, Ast)
import qualified Ocram.Transformation.Inline as Inline
import System.Environment (getArgs, getProgName)
import System.Exit (exitWith, ExitCode(ExitFailure))
import System.IO (stderr, hPutStrLn)

main :: IO ()
main = do
  argv <- getArgs 
  prg <- getProgName
  opt <- exitOnError "options" $ options prg argv 
  ast <- exitOnError "parser" =<< parse opt
  ana <- exitOnError "analysis" $ analysis ast
  trans <- exitOnError "options" $ select_transformation $ optScheme opt
  let (ast', ds) = trans ana ast
  pretty_print opt ast'
  write_debug_symbols opt ds
  return ()

type Transformation = CallGraph -> Ast -> (Ast, DebugSymbols)

select_transformation :: String -> Either [OcramError] Transformation
select_transformation "inline" = Right Inline.transformation
select_transformation s = Left [new_error 1 ("unknown compilation scheme \"" ++ s ++ "\".") Nothing]

exitOnError :: String -> Either [OcramError] a -> IO a
exitOnError module_ (Left es) = hPutStrLn stderr (show_errors module_ es) >> exitWith (ExitFailure 1)
exitOnError _ (Right x) = return x
