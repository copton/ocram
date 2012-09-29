{-# LANGUAGE GADTs #-}
module Ocram.Intermediate.Optimize
(
  optimize_ir
) where

import Ocram.Intermediate.Representation

optimize_ir :: (Label, Body) -> (Label, Body)
optimize_ir = id
