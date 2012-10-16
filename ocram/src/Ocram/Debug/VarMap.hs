module Ocram.Debug.VarMap where

-- imports {{{1
import Ocram.Ruab (TRow, FQN, Variable)

type VarMap' = [( -- {{{1
    Variable
  , (TRow, TRow) -- scope
  , FQN
  )]
