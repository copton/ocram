{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

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

type UpState = [(FunctionId, CFunDef)]

instance UpVisitor EmptyDownState UpState where
	upCFunDef fd _ _ = [((functionId' fd), fd)]

getFunctions :: Context -> FunctionMap
getFunctions ctx = fromList $ traverseCTranslUnit (ctxAst ctx) emptyDownState
