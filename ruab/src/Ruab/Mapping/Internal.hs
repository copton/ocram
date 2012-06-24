{-# LANGUAGE TemplateHaskell, ViewPatterns #-}
module Ruab.Mapping.Internal where

-- imports {{{1
import Data.List (find)
import Ocram.Ruab (LocMap, ELocation(..), TLocation(..))

map_ecode_row :: LocMap -> Int -> Maybe Int
map_ecode_row lm row = fmap (elocRow . snd) $ find ((row==) . tlocRow . fst) lm
