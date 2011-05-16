module Ocram.Transformation.Inline.Types where

import Language.C.Syntax.AST
import qualified Data.Map as Map
import Ocram.Types (Symbol)

type SymTab = Map.Map Symbol CDecl

data FunctionInfo = FunctionInfo {
		fiResultType :: CTypeSpec,
		fiVariables :: SymTab,
		fiBody :: CStat
	}

type FunctionInfos = Map.Map Symbol FunctionInfo
