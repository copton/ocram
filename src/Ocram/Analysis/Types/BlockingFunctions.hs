module Ocram.Analysis.Types.BlockingFunctions (
	BlockingFunctions
) where

import Data.Map as Map
import Language.C.Syntax.AST (CExtDecl)

-- map of all blocking function declarations
type BlockingFunctions = Map.Map String CExtDecl
