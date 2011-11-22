module Ocram.Output 
-- export {{{1
(
  pretty_print,
  write_debug_symbols
) where

-- import {{{1
import Ocram.Types (Ast, DebugSymbols)
import Ocram.Options (Options, optOutput)
import System.IO (openFile, IOMode(WriteMode), hClose, hPutStrLn)
import Language.C (pretty)

-- pretty_print :: Options -> Ast -> IO () {{{1
pretty_print :: Options -> Ast -> IO ()
pretty_print options ast = do
  let file = optOutput options
  let prelude = "#include <stddef.h>\n"
  let code = show $ pretty ast 
  let main = "int main() { return 0; }"
--  write file $ prelude ++ code ++ "\n" ++ main
  write file $ prelude ++ code

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

