module Ocram.Analysis.FunctionMap (
	getFunctions
) where

import Ocram.Types (AST, Result)
import Data.Map (fromList)
import Ocram.Visitor (UpVisitor(..), EmptyDownState, emptyDownState, traverseCTranslUnit)
import Ocram.Analysis.Types (FunctionMap)
import Ocram.Symbols (Symbol, symbol)
import Language.C.Syntax.AST (CFunDef)

getFunctions :: AST -> Result FunctionMap
getFunctions ast = return $ fromList $ snd $ traverseCTranslUnit ast emptyDownState

type UpState = [(Symbol, CFunDef)]

instance UpVisitor EmptyDownState UpState where
	upCFunDef fd _ _ = [((symbol fd), fd)]
