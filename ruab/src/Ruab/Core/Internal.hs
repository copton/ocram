module Ruab.Core.Internal where

import Control.Monad (guard)

import qualified Ocram.Ruab as R

t2p_row' :: R.PreprocMap -> R.TRow -> Maybe R.PRow -- {{{1
t2p_row'(R.PreprocMap _ prows locs) trow = do
  guard (trow > 0)
  let (src, dst) = (last . takeWhile ((<=trow) . fst)) locs
  let prow = dst + (R.PRow . R.getTRow) (trow - src + 1)
  guard (prow <= prows)
  return prow

p2t_row' :: R.PreprocMap -> R.PRow -> Maybe R.TRow -- {{{1
p2t_row' ppm@(R.PreprocMap trows _ locs) prow = do
  guard (prow > 0)
  let (src, dst) = (last . takeWhile ((<=prow) . snd)) locs
  let trow = src + (R.TRow . R.getPRow) (prow - dst - 1)
  guard (trow <= trows)
  prow' <- t2p_row' ppm trow
  guard (prow' == prow)
  return trow
