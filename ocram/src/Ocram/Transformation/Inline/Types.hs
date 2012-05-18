module Ocram.Transformation.Inline.Types
(
  Transformation, WR
)
where

import Control.Monad.Writer
import Language.C.Syntax.AST (CTranslationUnit)
import Ocram.Analysis (CallGraph)
import Ocram.Debug (VarMap)
import Ocram.Print (ENodeInfo)

type WR a = Writer VarMap a

type Transformation = CallGraph -> CTranslationUnit ENodeInfo -> WR (CTranslationUnit ENodeInfo)
