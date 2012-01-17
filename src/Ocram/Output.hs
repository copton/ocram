module Ocram.Output 
-- export {{{1
(
  pretty_print, dump_pal,
  write_debug_symbols
) where

-- import {{{1
import Ocram.Types (Ast, DebugSymbols)
import Ocram.Options (Options, optOutput, optPalHeader)
import System.IO (openFile, IOMode(WriteMode), hClose, hPutStrLn)
import Language.C (pretty)

-- pretty_print :: Options -> Ast -> IO () {{{1
pretty_print :: Options -> Ast -> IO ()
pretty_print options ast =
  write (optOutput options) $ show (pretty ast)

dump_pal :: Options -> Ast -> IO ()
dump_pal options ast =
  write (optPalHeader options) (show (pretty ast))

-- write_debug_symbols :: Options -> DebugSymbols -> IO () {{{1
write_debug_symbols :: Options -> DebugSymbols -> IO ()
write_debug_symbols _ _ = return ()

-- utils {{{1
write :: String -> String -> IO ()
write filename contents = do
    outh <- openFile filename WriteMode
    hPutStrLn outh contents
    hClose outh
    return ()

