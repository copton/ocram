module Ocram.Analysis.Types.FunctionMap (
	FunctionMap
) where

import qualified Data.Map as Map
import Language.C.Syntax.AST (CFunDef)

type FunctionMap = Map.Map String CFunDef
