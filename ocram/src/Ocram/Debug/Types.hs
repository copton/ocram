module Ocram.Debug.Types where

import Ocram.Ruab (ThreadId, TRow, ERow, FQN, Variable)

data Breakpoint = Breakpoint { -- {{{1
    bpTRow     :: TRow
  , bpERow     :: ERow
  , bpThread   :: Maybe ThreadId
  , bpBlocking :: Bool
  , bpFile     :: Maybe FilePath
  } deriving (Show, Eq)

type Breakpoints = [Breakpoint] -- {{{1

type VarMap' = [( -- {{{1
    Variable
  , (TRow, TRow) -- scope
  , FQN
  )]
