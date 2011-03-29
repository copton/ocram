module Ocram.Transformation.Tc2ec (tc2ec) where

import Ocram.Types (Result, getAst, ValidAst, OutputAst(OutputAst))
import Language.C.Syntax.AST (CTranslationUnit(CTranslUnit), CTranslUnit)
import Language.C.Data.Node (undefNode)

tc2ec :: ValidAst -> Result OutputAst
tc2ec _ = return $ OutputAst $ CTranslUnit [] undefNode
