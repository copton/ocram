module Ocram.Transformation.Inline (
	transform
) where

import Ocram.Types
import Ocram.Transformation.Inline.ControlFlow (transformControlFlow)
import Ocram.Transformation.Inline.DataFlow (transformDataFlow)

transform :: Context -> Result OutputAst
transform ctx = do
	valid_ast <- getValidAst ctx
	cg <- getCallGraph ctx
	cf <- getCriticalFunctions ctx
	bf <- getBlockingFunctions ctx
	sr <- getStartRoutines ctx
	df <- getDefinedFunctions ctx
	return $ OutputAst $ transformControlFlow bf cg cf sr $ transformDataFlow sr cg cf bf $ getAst valid_ast
