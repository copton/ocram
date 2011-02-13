module Context (
    Context(..)
) where

import Language.C.Syntax.AST (CTranslUnit)
import Analysis.Types

data Context = Context {
      ctxAst :: CTranslUnit
    , ctxFunctionMap :: Analysis.Types.FunctionMap
    , ctxStartRoutines :: Analysis.Types.StartRoutines
    , ctxCallGraph :: Analysis.Types.CallGraph
}

