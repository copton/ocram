{-# LANGUAGE TemplateHaskell, ViewPatterns #-}
module Ruab.Mapping.Internal where

-- imports {{{1
import Ruab.Util (abort)
import Text.Regex.Posix ((=~))

import qualified Data.ByteString.Char8 as BS

data PreprocMap = PreprocMap Int [(Int, Int)] deriving Show -- {{{1

preproc_map :: BS.ByteString -> PreprocMap -- {{{1
preproc_map ptcode
  | BS.null ptcode = PreprocMap 0 []
  | BS.last ptcode /= '\n' = $abort "pre-processed file should be terminated by a newline"
  | otherwise = PreprocMap (rows - 1) (reverse ppm)
  where
    (first:rest) = init $ BS.split '\n' ptcode
    (ppm, rows) = foldl go ([], 2) rest
    mainFile = case match first of
      (_, (BS.null -> True), _, _) -> $abort $ "unexpected first row in pre-processed file"
      (_, _, _, (_:file:_)) -> file
      x -> $abort $ "unexpected parameter: " ++ show x

    match :: BS.ByteString -> (BS.ByteString, BS.ByteString, BS.ByteString, [BS.ByteString])
    match txt = txt =~ "^# ([0-9]*) \"([^\"]+)\".*$" -- prefix, match, postfix, groups

    go (ppm', row) line = case match line of
      (_, (BS.null -> True), _, _ ) -> (ppm', row + 1)
      (_, _, _, (row':file:_)) -> if file == mainFile
        then (((read . BS.unpack) row', row) : ppm', row + 1)
        else (ppm', row + 1)
      x -> $abort $ "unexpected parameter:" ++ show x
