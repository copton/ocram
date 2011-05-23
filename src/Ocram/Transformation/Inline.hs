module Ocram.Transformation.Inline 
-- exports {{{1
(
	transform
) where

-- imports {{{1
import Ocram.Transformation.Inline.Step1 
import Ocram.Transformation.Inline.Step2
import Ocram.Transformation.Inline.Step3
import Ocram.Transformation.Inline.Step4
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
	let ast1 = getAst valid_ast
	let (ast2, bfs, cfs) = step1 bf cf ast1
	let fis = step2 bfs cfs
	let ast3 = step3 cf sr cg fis ast2
	let ast4 = step4 sr cg bf fis ast3
	return $ OutputAst $ ast4

