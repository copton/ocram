module Ruab.Core.Internal where

import Control.Monad (guard)
import Ocram.Ruab (PreprocMap(..))

t2p_row' :: PreprocMap -> Int -> Maybe Int -- {{{2
t2p_row'(PreprocMap _ prows locs) trow = do
  guard (trow > 0)
  let (src, dst) = (last . takeWhile ((<=trow) . fst)) locs
  let prow = dst + (trow - src) + 1
  guard (prow <= prows)
  return prow

p2t_row' :: PreprocMap -> Int -> Maybe Int -- {{{2
p2t_row' ppm@(PreprocMap trows _ locs) prow = do
  guard (prow > 0)
  let (src, dst) = (last . takeWhile ((<=prow) . snd)) locs
  let trow = src + (prow - dst) - 1
  guard (trow <= trows)
  prow' <- t2p_row' ppm trow
  guard (prow' == prow)
  return trow
