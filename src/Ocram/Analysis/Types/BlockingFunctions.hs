module Ocram.Analysis.Types.BlockingFunctions (
	BlockingFunctions
) where

import Data.Map as Map
import Language.C.Syntax.AST (CExtDecl)

type BlockingFunctions = Map.Map String CExtDecl
