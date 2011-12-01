module Ocram.Transformation.Inline 
-- exports {{{1
(
  transformation, compare_pal, extract_pal
) where

-- imports {{{1
import Control.Monad.Writer (runWriter)
import Ocram.Analysis (CallGraph)
import Ocram.Transformation.Inline.CriticalFunctions (addBlockingFunctionDecls, removeCriticalFunctions)
import Ocram.Transformation.Inline.Normalize (normalize)
import Ocram.Transformation.Inline.Pal (extract_pal, compare_pal)
import Ocram.Transformation.Inline.ThreadFunction (addThreadFunctions)
import Ocram.Transformation.Inline.TStack (addTStacks)
import Ocram.Transformation.Inline.Types
import Ocram.Transformation.Inline.UniqueIdentifiers (unique_identifiers)
import Ocram.Types (Ast, DebugSymbols)

transformation :: CallGraph -> Ast -> (Ast, DebugSymbols) -- {{{1
transformation cg ast = runWriter (transformation' cg ast)

transformation' :: Transformation
transformation' cg ast = return ast
  >>= unique_identifiers cg
  >>= normalize cg
  >>= addTStacks cg
  >>= addBlockingFunctionDecls cg
  >>= addThreadFunctions cg
  >>= removeCriticalFunctions cg
