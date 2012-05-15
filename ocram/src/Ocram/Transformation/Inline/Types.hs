module Ocram.Transformation.Inline.Types where

import Control.Monad.Writer
import Language.C.Syntax.AST (CTranslUnit)
import Ocram.Analysis (CallGraph)
import Ocram.Debug (VarMap)

type WR a = Writer VarMap a

type Transformation = CallGraph -> CTranslUnit -> WR CTranslUnit
