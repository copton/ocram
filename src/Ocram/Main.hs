module Ocram.Main (main) where

import Control.Monad (when)
import Ocram.Analysis (analysis)
import Ocram.Options (options, Options(..), PalAction(Compare, Dump))
import Ocram.Output (pretty_print, write_debug_symbols, dump_pal)
import Ocram.Parser (parse)
import Ocram.Text (OcramError, show_errors)
import qualified Ocram.Transformation.Inline as Inline
import System.Environment (getArgs, getProgName)
import System.Exit (exitWith, ExitCode(ExitFailure))
import System.IO (stderr, hPutStrLn)

main :: IO ()
main = do
  argv <- getArgs 
  prg <- getProgName
  opt <- exitOnError "options" $ options prg argv 
  ast <- exitOnError "parser" =<< parse opt (optInput opt)
  ana <- exitOnError "analysis" $ analysis ast
  let (ast', ds) = Inline.transformation ana ast
  when (not $ null $ optPalHeader opt) $
    case optPalAction opt of
      Compare -> do
        palAst <- exitOnError "parser" =<< parse opt (optPalHeader opt)
        exitOnError "transformation" $ Inline.compare_pal ana ast' palAst
      Dump -> dump_pal opt (Inline.extract_pal ana ast')
  pretty_print opt ast'
  write_debug_symbols opt ds
  return ()

exitOnError :: String -> Either [OcramError] a -> IO a
exitOnError module_ (Left es) = hPutStrLn stderr (show_errors module_ es) >> exitWith (ExitFailure 1)
exitOnError _ (Right x) = return x
