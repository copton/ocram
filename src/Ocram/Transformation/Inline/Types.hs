module Ocram.Transformation.Inline.Types where

import Language.C.Syntax.AST
import qualified Data.Map as Map
import Ocram.Types (Symbol)

data FunctionInfo = FunctionInfo {
		fiResultType :: CTypeSpec,
		fiVariables :: [CDecl],
		fiBody :: CStat
	}

type FunctionInfos = Map.Map Symbol FunctionInfo
