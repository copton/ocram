{-# LANGUAGE TemplateHaskell, ViewPatterns #-}
module Ocram.Debug.Internal where

import Ocram.Ruab (PreprocMap(..), TRow(..))
import Ocram.Util (abort)
import Text.Regex.Posix ((=~))

import qualified Data.ByteString.Char8 as BS

preproc_map :: BS.ByteString -> BS.ByteString -> PreprocMap -- {{{1
preproc_map tcode pcode
  | BS.null pcode = PreprocMap 0 0 []
  | BS.last pcode /= '\n' = $abort "pre-processed file should be terminated by a newline"
  | otherwise = PreprocMap (trows) (prows - 1) (reverse ppm)
  where
    trows = TRow $ length $ BS.split '\n' tcode
    (first:rest) = init $ BS.split '\n' pcode
    (ppm, prows) = foldl go ([], 2) rest
    mainFile = case match first of
      (_, (BS.null -> True), _, _) -> $abort $ "unexpected first row in pre-processed file"
      (_, _, _, (_:file:_)) -> file
      x -> $abort $ "unexpected parameter: " ++ show x

    match :: BS.ByteString -> (BS.ByteString, BS.ByteString, BS.ByteString, [BS.ByteString])
    match txt = txt =~ "^# ([0-9]*) \"([^\"]+)\".*$" -- prefix, match, postfix, groups

    go (ppm', row) line = case match line of
      (_, (BS.null -> True), _, _ ) -> (ppm', row + 1)
      (_, _, _, (row':file:_)) -> if file == mainFile
        then (((TRow . read . BS.unpack) row', row) : ppm', row + 1)
        else (ppm', row + 1)
      x -> $abort $ "unexpected parameter:" ++ show x