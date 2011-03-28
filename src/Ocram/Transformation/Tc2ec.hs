module Ocram.Transformation.Tc2ec (tc2ec) where

import Ocram.Types (Result, AST)
import Language.C.Syntax.AST (CTranslationUnit(CTranslUnit), CTranslUnit)
import Language.C.Data.Node (undefNode)

tc2ec :: AST -> Result AST
tc2ec _ = return $ CTranslUnit [] undefNode
