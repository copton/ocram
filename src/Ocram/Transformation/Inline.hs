module Ocram.Transformation.Inline 
-- exports {{{1
(
  transformation
) where

-- imports {{{1
import Ocram.Transformation.Inline.TStack (addTStacks)
import Ocram.Transformation.Inline.ThreadFunction (addThreadFunctions)
import Ocram.Transformation.Inline.Finalize (finalize)
import Ocram.Transformation.Inline.Types
import Ocram.Types (Ast, DebugSymbols)
import Ocram.Analysis (CallGraph)

-- transformation :: Analysis -> Ast -> (Ast, DebugSymbols) {{{1
transformation :: CallGraph -> Ast -> (Ast, DebugSymbols)
transformation cg ast = execWR cg (transformation' ast)

transformation' :: Ast -> WR Ast
transformation' ast = return ast
  >>= addTStacks
  >>= addThreadFunctions
  >>= finalize
