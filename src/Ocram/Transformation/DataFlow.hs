module Ocram.Transformation.DataFlow (transformDataFlow) where

import Ocram.Types(ValidAst, Result, StacklessAst)
import Ocram.Analysis (CriticalFunctions, FunctionMap)

transformDataFlow :: ValidAst -> CriticalFunctions -> FunctionMap -> Result StacklessAst
transformDataFlow valid_ast cf fm = undefined
