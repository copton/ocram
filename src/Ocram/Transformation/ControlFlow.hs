module Ocram.Transformation.ControlFlow (
	transformControlFlow
) where

import Ocram.Transformation.Types (FunctionInfos)
import Ocram.Transformation.ControlFlow.RemoveCriticalFunctions (removeCriticalFunctions)
import Ocram.Transformation.ControlFlow.AddHandlerFunction (addHandlerFunction)
import Ocram.Analysis (CriticalFunctions, FunctionMap)
import Ocram.Types (Result, StacklessAst, OutputAst(OutputAst), getAst)

transformControlFlow :: StacklessAst -> FunctionInfos -> CriticalFunctions -> FunctionMap -> Result OutputAst
transformControlFlow ast fi cf fm = return $ OutputAst $ addHandlerFunction cf fm $ removeCriticalFunctions cf $ getAst ast
