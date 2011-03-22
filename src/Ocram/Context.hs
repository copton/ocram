module Ocram.Context (
	Context(..)
) where

import Language.C.Syntax.AST (CTranslUnit)
import Ocram.Analysis.Types

data Context = Context {
	  ctxInputAst :: CTranslUnit
	, ctxFunctionMap :: Ocram.Analysis.Types.FunctionMap
	, ctxStartRoutines :: Ocram.Analysis.Types.StartRoutines
	, ctxCallGraph :: Ocram.Analysis.Types.CallGraph
	, ctxBlockingFunctions :: Ocram.Analysis.Types.BlockingFunctions
	, ctxCriticalFunctions :: Ocram.Analysis.Types.CriticalFunctions
	, ctxOutputAst :: CTranslUnit
}

