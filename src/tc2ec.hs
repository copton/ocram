module Main where
import System.Environment (getArgs, getProgName)
import System.Exit (exitWith, ExitCode(ExitFailure))
import System.IO (hPutStrLn, stderr)
import Control.Monad (when)
import Text.PrettyPrint.HughesPJ (text, render, (<+>), hsep)

import Language.C (parseCFile, pretty)
import Language.C.System.GCC (newGCC)

import Analysis (findStartRoutines)
import Analysis.CallGraph (determineCallGraph)
import Util.Names (functionName)

usageMsg :: String -> String
usageMsg prg = render $
  text "Usage:" <+> text prg <+> hsep (map text ["CPP_OPTIONS","input_file.c"])

main :: IO ()
main = do
    progName <- getProgName
    args <- getArgs
    let usageErr = (hPutStrLn stderr (usageMsg progName) >> exitWith (ExitFailure 1))
    when (length args < 1) usageErr
    let (opts,input_file) = (init args, last args)

    ast <- errorOnLeftM "Parse Error" $ parseCFile (newGCC "gcc") Nothing opts input_file
    let sr = findStartRoutines ast
    let cg = determineCallGraph ast
    putStrLn $ show $ map functionName sr
--    print $ pretty ast

errorOnLeft :: (Show a) => String -> (Either a b) -> IO b
errorOnLeft msg = either (error . ((msg ++ ": ")++).show) return
errorOnLeftM :: (Show a) => String -> IO (Either a b) -> IO b
errorOnLeftM msg action = action >>= errorOnLeft msg
