module Ocram.Transformation.Inline 
-- exports {{{1
(
  transformation
) where

-- imports {{{1
import Ocram.Analysis (CallGraph)
import Ocram.Transformation.Inline.Finalize (finalize)
import Ocram.Transformation.Inline.Normalize (normalize)
import Ocram.Transformation.Inline.ThreadFunction (addThreadFunctions)
import Ocram.Transformation.Inline.TStack (addTStacks)
import Ocram.Transformation.Inline.Types
import Ocram.Transformation.Inline.UniqueIdentifiers (unique_identifiers)
import Ocram.Types (Ast, DebugSymbols)

-- transformation :: Analysis -> Ast -> (Ast, DebugSymbols) {{{1
transformation :: CallGraph -> Ast -> (Ast, DebugSymbols)
transformation cg ast = execWR cg (transformation' ast)

transformation' :: Ast -> WR Ast
transformation' ast = return ast
  >>= unique_identifiers
  >>= normalize
  >>= addTStacks
  >>= addThreadFunctions
  >>= finalize
