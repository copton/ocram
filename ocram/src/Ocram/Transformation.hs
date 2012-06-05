module Ocram.Transformation
-- exports {{{1
(

  transformation, enodify
) where

-- imports {{{1
import Control.Monad.Writer (runWriter)
import Language.C.Syntax.AST
import Language.C.Data.Node (NodeInfo)
import Ocram.Analysis (CallGraph)
import Ocram.Debug (enrichNodeInfo, ENodeInfo, VarMap)
import Ocram.Transformation.Normalize (normalize)
import Ocram.Transformation.Translate (translate)
import Ocram.Transformation.Pal (extract_pal)
import Ocram.Transformation.Types (Transformation)

enodify :: CTranslationUnit NodeInfo -> CTranslationUnit ENodeInfo -- {{{1
enodify = fmap enrichNodeInfo

transformation :: CallGraph -> CTranslationUnit NodeInfo -> (CTranslationUnit ENodeInfo, CTranslationUnit ENodeInfo, VarMap) -- {{{1
transformation cg ast =
  let
    (ast', ds) = runWriter $ transformation' cg $ enodify ast
    pal = extract_pal cg ast'
  in
    (ast', pal, ds)

transformation' :: Transformation
transformation' cg ast = return ast
  >>= normalize cg
  >>= translate cg
