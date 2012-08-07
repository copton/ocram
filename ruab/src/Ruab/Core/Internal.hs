{-# LANGUAGE TemplateHaskell #-}
module Ruab.Core.Internal where

import Control.Monad (guard)
import Data.List (find)
import Ruab.Util (fromJust_s)

import qualified Ocram.Ruab as R

-- types {{{1
type PRow = Int
type TRow = Int
type ERow = Int
type ThreadId = Int

data ERowMatch
  = NoMatch
  | NonCritical ERow
  | Critical [(ThreadId, ERow)]

t2p_row' :: R.PreprocMap -> TRow -> Maybe PRow -- {{{1
t2p_row'(R.PreprocMap _ prows locs) trow = do
  guard (trow > 0)
  let (src, dst) = (last . takeWhile ((<=trow) . fst)) locs
  let prow = dst + (trow - src) + 1
  guard (prow <= prows)
  return prow

p2t_row' :: R.PreprocMap -> PRow -> Maybe TRow -- {{{1
p2t_row' ppm@(R.PreprocMap trows _ locs) prow = do
  guard (prow > 0)
  let (src, dst) = (last . takeWhile ((<=prow) . snd)) locs
  let trow = src + (prow - dst) - 1
  guard (trow <= trows)
  prow' <- t2p_row' ppm trow
  guard (prow' == prow)
  return trow

t2e_row' :: R.LocMap -> String -> TRow -> ERowMatch -- {{{1
t2e_row' lm tfile row =
  let
    locs = filter ((row==) . R.tlocRow . R.locTloc) $
           filter ((tfile==) . R.tlocFile . R.locTloc) $
           lm
  in
    case locs of
      []                             -> NoMatch
      [R.Location _ eloc _ Nothing]  -> NonCritical $ R.elocRow eloc
      [R.Location _ eloc _ (Just t)] -> Critical [(t, R.elocRow eloc)]
      elocs                          -> Critical $ flip map elocs (\loc ->
          (
            ($fromJust_s . R.locThreadId) loc
          , (R.elocRow . R.locEloc) loc
          )
        )

e2t_row' :: R.LocMap -> String -> ERow -> Maybe TRow -- {{{1
e2t_row' lm tfile row =
  fmap (R.tlocRow . R.locTloc) $
  find ((row==) . R.elocRow . R.locEloc) $
  filter ((tfile==) . R.tlocFile . R.locTloc) $
  lm
