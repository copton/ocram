module Ocram.Transformation.ControlFlow (
	transformControlFlow
) where

import Ocram.Transformation.ControlFlow.RemoveCriticalFunctions (removeCriticalFunctions)
import Ocram.Transformation.ControlFlow.AddHandlerFunction (addHandlerFunction)
import Ocram.Analysis (CriticalFunctions, FunctionMap)
import Ocram.Types (Result, StacklessAst, OutputAst(OutputAst), getAst)

transformControlFlow :: StacklessAst -> CriticalFunctions -> FunctionMap -> Result OutputAst
transformControlFlow ast cf fm = return $ OutputAst $ addHandlerFunction cf fm $ removeCriticalFunctions cf $ getAst ast
