{-# LANGUAGE TemplateHaskell #-}
module Ruab.Backend.GDB
(
    backend_start
  , backend_quit
  , set_breakpoint
  , GDB
) where

import Data.List (intercalate)
import Prelude hiding (interact)
import Ruab.Backend.GDB.Commands
import Ruab.Backend.GDB.Representation
import Ruab.Backend.GDB.IO
import Ruab.Util (abort)

backend_start :: FilePath -> IO (Either String GDB)
backend_start binary = do
  res <- start Nothing
  case res of
    Left e -> (return . Left) e 
    Right (gdb, _) -> do
      output <- interact gdb $ MICommand Nothing "file-exec-and-symbols" [Option binary Nothing] []
      return $ failOrSucceed (checkOutput output) gdb

backend_quit :: GDB -> IO ()
backend_quit = quit

set_breakpoint :: GDB -> FilePath -> Int -> IO (Either String ())
set_breakpoint gdb file row = do
  output <- interact gdb $ MICommand Nothing "break-insert" [Option (file ++ ":" ++ show row) Nothing] []
  return $ failOrSucceed (checkOutput output) ()



checkOutput :: Output -> Maybe String
checkOutput (Output _ (Just (ResultRecord _ RCDone _))) = Nothing
checkOutput (Output _ (Just (ResultRecord _ RCError rs))) =
  Just $ intercalate "\n" $ map getMsg $ filter isMsg rs
  where
    isMsg (Result var _) = var == "msg"
    getMsg (Result _ (VConst str)) = str :: String
    getMsg o = $abort $ "unexpected parameter" ++ show o

checkOutput o = $abort $ "unexpected parameter" ++ show o

failOrSucceed :: Maybe a -> b -> Either a b
failOrSucceed Nothing  x = Right x
failOrSucceed (Just x) _ = Left x
