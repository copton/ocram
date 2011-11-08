module Ocram.Transformation.Inline.Types where

import Control.Monad.Writer
import Ocram.Types (DebugSymbols, Ast)
import Ocram.Analysis (CallGraph)

type WR a = Writer DebugSymbols a

type Transformation = CallGraph -> Ast -> WR Ast
