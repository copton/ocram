module Ruab.Core.Internal where

import Control.Monad (guard)
import Data.List (find)

import qualified Ocram.Ruab as R

t2p_row' :: R.PreprocMap -> Int -> Maybe Int -- {{{1
t2p_row'(R.PreprocMap _ prows locs) trow = do
  guard (trow > 0)
  let (src, dst) = (last . takeWhile ((<=trow) . fst)) locs
  let prow = dst + (trow - src) + 1
  guard (prow <= prows)
  return prow

p2t_row' :: R.PreprocMap -> Int -> Maybe Int -- {{{1
p2t_row' ppm@(R.PreprocMap trows _ locs) prow = do
  guard (prow > 0)
  let (src, dst) = (last . takeWhile ((<=prow) . snd)) locs
  let trow = src + (prow - dst) - 1
  guard (trow <= trows)
  prow' <- t2p_row' ppm trow
  guard (prow' == prow)
  return trow

p2e_row' :: R.LocMap -> String -> Int -> Maybe Int
p2e_row' lm tfile row = 
  fmap (R.elocRow . R.locEloc) $
  find ((row==) . R.tlocRow . R.locTloc) $
  filter ((tfile==) . R.tlocFile . R.locTloc) $
  lm

e2p_row' :: R.LocMap -> String -> Int -> Maybe Int
e2p_row' lm tfile row =
  fmap (R.tlocRow . R.locTloc) $
  find ((row==) . R.elocRow . R.locEloc) $
  filter ((tfile==) . R.tlocFile . R.locTloc) $
  lm
