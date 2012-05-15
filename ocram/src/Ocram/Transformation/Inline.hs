module Ocram.Transformation.Inline 
-- exports {{{1
(
  transformation
) where

-- imports {{{1
import Control.Monad.Writer (runWriter)
import Language.C.Syntax.AST (CTranslUnit)
import Ocram.Analysis (CallGraph)
import Ocram.Debug (VarMap)
import Ocram.Transformation.Inline.CriticalFunctions (addBlockingFunctionDecls, removeCriticalFunctions)
import Ocram.Transformation.Inline.Normalize (normalize)
import Ocram.Transformation.Inline.Pal (extract_pal)
import Ocram.Transformation.Inline.ThreadFunction (addThreadFunctions)
import Ocram.Transformation.Inline.TStack (addTStacks)
import Ocram.Transformation.Inline.Types
import Ocram.Transformation.Inline.UniqueIdentifiers (unique_identifiers)

transformation :: CallGraph -> CTranslUnit -> (CTranslUnit, CTranslUnit, VarMap) -- {{{1
transformation cg ast =
  let
    (ast', ds) = runWriter (transformation' cg ast)
    pal = extract_pal cg ast'
  in
    (ast', pal, ds)

transformation' :: Transformation
transformation' cg ast = return ast
  >>= normalize cg
  >>= unique_identifiers cg
  >>= addTStacks cg
  >>= addBlockingFunctionDecls cg
  >>= addThreadFunctions cg
  >>= removeCriticalFunctions cg
