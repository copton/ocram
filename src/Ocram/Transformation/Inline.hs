module Ocram.Transformation.Inline (
	transform
) where

import Ocram.Types
import Ocram.Transformation.Inline.ControlFlow (transformControlFlow)
import Ocram.Transformation.Inline.DataFlow (transformDataFlow)
import Ocram.Transformation.Util (removeAttributes)

transform :: Context -> Result OutputAst
transform ctx = do
	valid_ast <- getValidAst ctx
	cg <- getCallGraph ctx
	cf <- getCriticalFunctions ctx
	bf <- getBlockingFunctions ctx
	sr <- getStartRoutines ctx
	df <- getDefinedFunctions ctx
	return $ OutputAst $ transformControlFlow cf df $ transformDataFlow cg cf bf $ removeAttributes bf sr $ getAst valid_ast
