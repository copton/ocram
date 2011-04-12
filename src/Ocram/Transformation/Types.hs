module Ocram.Transformation.Types (
	Frame, FunctionInfo(..), FunctionInfos
) where

import Language.C.Syntax.AST
import Ocram.Symbols (Symbol)
import Data.Map as Map

type Frame = [CDecl]

data FunctionInfo = FunctionInfo {
	getResultType :: CTypeSpec,
	getTStackFrame :: Frame,
	getBody :: [CBlockItem]
}

type FunctionInfos = Map Symbol FunctionInfo
