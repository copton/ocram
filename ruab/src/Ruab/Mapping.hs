{-# LANGUAGE ViewPatterns, TemplateHaskell #-} 

module Ruab.Mapping
-- exports {{{1
(
    TextPosition(..)
-- preprocessor
  , PreprocMap
  , preproc_map
  , map_preprocessed_row
) where

-- imports {{{1
import Ruab.Util (abort)
import Text.Regex.Posix ((=~))

import qualified Data.ByteString.Char8 as BS

data TextPosition = TextPosition {
  tpRow :: Int,
  tpCol :: Int
  }

-- preprocessor {{{1
map_preprocessed_row :: PreprocMap -> Int -> Maybe Int -- {{{2
map_preprocessed_row (PreprocMap rows locs) row
  | row <= 0 = Nothing
  | otherwise =
    let
      (src, dst) = (last . takeWhile ((<=row) . fst)) locs
      res = dst + (row - src) + 1
    in
      if res > rows
        then Nothing
        else Just res

data PreprocMap = PreprocMap Int [(Int, Int)] deriving Show -- {{{2

preproc_map :: BS.ByteString -> PreprocMap -- {{{2
preproc_map ptcode
  | BS.null ptcode = PreprocMap 0 []
  | BS.last ptcode /= '\n' = preproc_map (BS.snoc ptcode '\n')
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
