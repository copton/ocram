module Ocram.Transformation.DataFlow (
	transformDataFlow
) where

import Ocram.Types (ValidAst, Result, StacklessAst)
import Ocram.Analysis (CriticalFunctions, FunctionMap)
import Ocram.Transformation.Types (FunctionInfos)
import Ocram.Transformation.DataFlow.FunctionInfo (retrieveFunctionInfos)

transformDataFlow :: ValidAst -> CriticalFunctions -> FunctionMap -> Result (FunctionInfos, StacklessAst)
transformDataFlow valid_ast cf fm = undefined

