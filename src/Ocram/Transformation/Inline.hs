module Ocram.Transformation.Inline 
-- exports {{{1
(
	transformation
) where
transformation = undefined

---- imports {{{1
--import Ocram.Transformation.Inline.Step1 
--import Ocram.Transformation.Inline.Step2
--import Ocram.Transformation.Inline.Step3
--import Ocram.Transformation.Inline.Step4
--import Ocram.Transformation.Inline.Types
--import Ocram.Types
--
---- transformation :: Analysis -> Ast -> (Ast, DebugSymbols) {{{1
--transformation :: Analysis -> Ast -> (Ast, DebugSymbols)
--transformation ana ast = execWR ana (transformation' ast)
--
--transformation' :: Ast -> WR Ast
--transformation' ast = do
--	(ast1, bfs, cfs) <- step1 ast
--	fis <- step2 bfs cfs
--	ast2 <- step3 fis ast1
--	ast3 <- step4 fis ast2
--	return ast3
