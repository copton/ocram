module Ocram.Analysis.FunctionMap (
	getFunctions
) where

import Ocram.Types (Result, getAst, SaneAst, FunctionMap, Symbol)
import Data.Map (fromList)
import Ocram.Visitor (UpVisitor(..), EmptyDownState, emptyDownState, traverseCTranslUnit)
import Ocram.Symbols (symbol)
import Language.C.Syntax.AST (CFunDef)

getFunctions :: SaneAst -> Result FunctionMap
getFunctions sane_ast = return $ fromList $ snd $ traverseCTranslUnit (getAst sane_ast) emptyDownState

type UpState = [(Symbol, CFunDef)]

instance UpVisitor EmptyDownState UpState where
	upCFunDef fd _ _ = [((symbol fd), fd)]
