{-# LANGUAGE MultiParamTypeClasses #-}

module Ocram.Analysis.Algorithms.FunctionMap (
	getFunctions
) where

import Data.Map (fromList)
import Ocram.Visitor (UpVisitor(..), EmptyDownState, emptyDownState, traverseCTranslUnit)
import Ocram.Util.Names (functionName)
import Ocram.Context (Context, ctxAst)
import Ocram.Analysis.Types.FunctionMap
import Data.Monoid (Monoid, mempty, mappend)
import Language.C.Syntax.AST (CFunDef)

newtype UpState = UpState {
	stFunctions :: [(FunctionId, CFunDef)]
}

instance Monoid UpState where
	mempty = UpState []
	(UpState m) `mappend` (UpState m') = UpState $ m ++ m'

instance UpVisitor EmptyDownState UpState where
	upCFunDef fd _ _ = UpState $ [((functionId' fd), fd)]

getFunctions :: Context -> FunctionMap
getFunctions ctx = fromList $ stFunctions $ traverseCTranslUnit (ctxAst ctx) emptyDownState
