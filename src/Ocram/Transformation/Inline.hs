module Ocram.Transformation.Inline 
-- exports {{{1
(
  transformation
) where

-- imports {{{1
import Control.Monad.Writer (runWriter)
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
transformation cg ast = runWriter (transformation' cg ast)

transformation' :: Transformation
transformation' cg ast = return ast
  >>= normalize cg
  >>= unique_identifiers cg
  >>= addTStacks cg
  >>= addThreadFunctions cg
  >>= finalize cg


------------------------
-- Transformation steps
-------------------------
-- normalize:
--   - wrap dangling statements into compound statements
--   - force all critical calls into one of the three following forms:
--     - call();
--     - expression = call();
--     - type variable_1, ... , variable_k = call(), ..., variable_n;
--   - may introduce new temporary variables
--
-- unique_identifiers:
--   - rename identifiers so that no shadowing occurs any more
--
-- addTStacks
--   - add TStack structures and instances
--
-- addThreadFunctions
--   - add thread functions
--     - inline critical functions
--     - remove local variable declarations
--       - preserve initialization
--     - rewrite access to local variables (use t-stacks)
--     - replace critical call with split phase
--
-- finalize
--   - remove original critical functions
--   - rewrite declaration of blocking functions
