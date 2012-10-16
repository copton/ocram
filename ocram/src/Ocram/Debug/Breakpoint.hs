module Ocram.Debug.Breakpoint where

import Ocram.Ruab (ThreadId, TRow, ERow)

data Breakpoint = Breakpoint { -- {{{1
    bpTRow     :: TRow
  , bpERow     :: ERow
  , bpThread   :: Maybe ThreadId
  , bpBlocking :: Bool
  , bpFile     :: Maybe FilePath
  } deriving (Show, Eq)

type Breakpoints = [Breakpoint] -- {{{1
