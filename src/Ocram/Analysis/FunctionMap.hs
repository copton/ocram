module Ocram.Analysis.FunctionMap (
	getFunctions
) where

import Ocram.Types (Result, getAst, SaneAst)
import Data.Map (fromList)
import Ocram.Visitor (UpVisitor(..), EmptyDownState, emptyDownState, traverseCTranslUnit)
import Ocram.Analysis.Types (FunctionMap)
import Ocram.Symbols (Symbol, symbol)
import Language.C.Syntax.AST (CFunDef)

getFunctions :: SaneAst -> Result FunctionMap
getFunctions sane_ast = return $ fromList $ snd $ traverseCTranslUnit (getAst sane_ast) emptyDownState

type UpState = [(Symbol, CFunDef)]

instance UpVisitor EmptyDownState UpState where
	upCFunDef fd _ _ = [((symbol fd), fd)]
