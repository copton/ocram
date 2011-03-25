module Ocram.Analysis.FunctionMap (
	getFunctions
) where

import Data.Map (fromList)
import Ocram.Visitor (UpVisitor(..), EmptyDownState, emptyDownState, traverseCTranslUnit)
import Ocram.Context (Context, ctxInputAst)
import Ocram.Analysis.Types (FunctionMap)
import Ocram.Symbols (Symbol, symbol)
import Language.C.Syntax.AST (CFunDef)

type UpState = [(Symbol, CFunDef)]

instance UpVisitor EmptyDownState UpState where
	upCFunDef fd _ _ = [((symbol fd), fd)]

getFunctions :: Context -> FunctionMap
getFunctions ctx = fromList $ snd $ traverseCTranslUnit (ctxInputAst ctx) emptyDownState
