module Ocram.Transformation.Inline 
-- exports {{{1
(
	transform
) where

-- imports {{{1
import Ocram.Transformation.Inline.Step1 
import Ocram.Transformation.Inline.Step2
import Ocram.Types

-- transform {{{1
transform :: Context -> Result OutputAst
transform ctx = do
	valid_ast <- getValidAst ctx
	cg <- getCallGraph ctx
	cf <- getCriticalFunctions ctx
	bf <- getBlockingFunctions ctx
	sr <- getStartRoutines ctx
	df <- getDefinedFunctions ctx
	let ast = getAst valid_ast
	let (ast', fis) = step1 bf cf ast
	let ast'' = step2 sr cg fis ast'
	return $ OutputAst $ ast''

