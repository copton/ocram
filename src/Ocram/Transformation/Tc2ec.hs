module Ocram.Transformation.Tc2ec (tc2ec) where

import Language.C.Syntax.AST (CTranslationUnit(CTranslUnit), CTranslUnit)
import Ocram.Context (Context)
import Language.C.Data.Node (undefNode)

tc2ec :: Context -> CTranslUnit
tc2ec _ = CTranslUnit [] undefNode
