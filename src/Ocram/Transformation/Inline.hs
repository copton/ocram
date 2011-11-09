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
  >>= unique_identifiers cg
  >>= normalize cg
  >>= addTStacks cg
  >>= addThreadFunctions cg
  >>= finalize cg


------------------------
-- Transformation steps
-------------------------
-- unique_identifiers:
--   - rename identifiers so that
--    - for each function no two identifiers have the same name
--    - no shadowing occurs
--
-- normalize:
--   - wrap dangling statements into compound statements
--   - force all critical calls into normal form
--     - possible forms:    
--       - call();
--       - expression = call();
--       - type variable_k = call();
--     - may introduce new temporary variables (with unique names)
--   - exactly one identifier per declaration
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
