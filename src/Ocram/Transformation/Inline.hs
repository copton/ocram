module Ocram.Transformation.Inline 
-- exports {{{1
(
  transformation
) where

-- imports {{{1
import Ocram.Transformation.Inline.Step1 
import Ocram.Transformation.Inline.Step2
import Ocram.Transformation.Inline.Step3
import Ocram.Transformation.Inline.Types
import Ocram.Types (Ast, DebugSymbols)
import Ocram.Analysis (CallGraph)

-- transformation :: Analysis -> Ast -> (Ast, DebugSymbols) {{{1
transformation :: CallGraph -> Ast -> (Ast, DebugSymbols)
transformation cg ast = execWR cg (transformation' ast)

transformation' :: Ast -> WR Ast
transformation' ast = return ast >>= step1 >>= step2 >>= step3 >>= return
