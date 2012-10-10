{-# LANGUAGE TemplateHaskell #-}
module Ocram.IO
-- export {{{1
(
    parse
  , dump_ecode
  , dump_debug_info
  , generate_pal
) where

-- import {{{1
import Control.Exception (handle, IOException)
import Data.Maybe (fromMaybe)
import Data.List (intersperse)
import Language.C.Data.Position (initPos)
import Language.C.Parser (parseC)
import Language.C.Syntax.AST (CTranslUnit)
import Ocram.Analysis (Footprint)
import Ocram.Options (Options(..))
import Ocram.Debug (CTranslUnit')
import Ocram.Text (OcramError, new_error)
import Ocram.Print (render')
import Ocram.Util (fromJust_s, abort)
import System.Exit (ExitCode(..))
import System.IO (openFile, IOMode(WriteMode), hClose, hGetContents)
import System.Process (createProcess, proc, StdStream(CreatePipe), CreateProcess(std_out, std_in, std_err), waitForProcess)

import qualified Data.ByteString.Char8 as BS

parse :: Options -> IO (Either [OcramError] (BS.ByteString, BS.ByteString, CTranslUnit)) -- {{{1
parse opt = do
  tcode <- BS.readFile infile
  runOcramError $ do
    pcode <- IoOcramError $ exec ((words (optPreprocessor opt)) ++ [infile]) Nothing
    ast <- parseC' pcode
    return (tcode, pcode, ast)
  where
    infile = optInput opt
    parseC' code = case parseC code (initPos infile) of
      Left e -> (IoOcramError . return . Left) [new_error 1 ("parsing failed\n" ++ show e) Nothing]
      Right x -> return x


dump_ecode :: Options -> BS.ByteString -> IO (Either [OcramError] ()) -- {{{1
dump_ecode options ecode =
  write (optOutput options) $ ecode `BS.append` BS.singleton '\n'

dump_debug_info :: Options -> BS.ByteString -> IO (Either [OcramError] ()) -- {{{1
dump_debug_info opt di = case optDebugFile opt of
  Nothing -> (return . Right) ()
  Just file -> write file di

generate_pal :: Options -> Footprint -> CTranslUnit' -> IO (Either [OcramError] ()) -- {{{1
generate_pal options fpr header = case optPalGenerator options of
  Nothing -> (return . Right) ()
  Just generator ->
    let
      target = $fromJust_s (optPalFile options) 
      args = map (concat . intersperse ",") fpr
      input = render' header
    in
      runOcramError $ do
        pal <- IoOcramError $ exec (generator:args) (Just input)
        IoOcramError $ write target pal
      

-- utils {{{1
write :: String -> BS.ByteString -> IO (Either [OcramError] ()) -- {{{2
write filename contents = handle exception $ do
    outh <- openFile filename WriteMode
    BS.hPutStr outh contents
    hClose outh
    (return . Right) ()
    where
      exception :: IOException -> IO (Either [OcramError] ())
      exception e = (return . Left) [new_error 1 ("failed to write to '" ++ filename ++ "':\n" ++ show e) Nothing]

exec :: [String] -> Maybe BS.ByteString -> IO (Either [OcramError] BS.ByteString) -- {{{2
exec (cmd:args) input = do
  let process = (proc cmd args) {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe}
  (Just hin, Just hout, Just herr, p) <- createProcess process
  BS.hPutStr hin $ fromMaybe BS.empty input
  hClose hin
  output <- BS.hGetContents hout
  ec <- waitForProcess p
  case ec of
    ExitSuccess -> (return . Right) output
    ExitFailure code -> do
      es <- hGetContents herr
      (return . Left) [new_error 2 ("calling '" ++ cmd ++ "' failed: (" ++ (show code) ++ ")\n:" ++ es) Nothing]
exec _ _ = $abort "unexpected parameter for exec"

newtype IoOcramError a = -- {{{2
  IoOcramError { runOcramError :: IO (Either [OcramError] a) }

instance Monad IoOcramError where
  return = IoOcramError . return . Right
  (>>=) value func = IoOcramError $ do
    value' <- runOcramError value
    case value' of
      Left e -> (return . Left) e
      Right value'' -> runOcramError $ func value''
