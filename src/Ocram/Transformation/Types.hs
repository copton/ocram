module Ocram.Transformation.Types (
	Frame, FunctionInfo(..), FunctionInfos, TStackFrame
) where

import Language.C.Syntax.AST
import Ocram.Symbols (Symbol)
import Data.Map as Map

type Frame = [CDecl]

data FunctionInfo = FunctionInfo {
	getFunctionName :: Symbol,
	getResultType :: CTypeSpec,
	getTStackFrame :: Frame,
	getBody :: [CBlockItem]
}

type FunctionInfos = Map Symbol FunctionInfo

type TStackFrame = CExtDecl
