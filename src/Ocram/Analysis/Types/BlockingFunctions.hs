module Ocram.Analysis.Types.BlockingFunctions (
	BlockingFunctions
)

import Data.Map as Map
import Language.C.Syntax.AST (CDeclExt)

type BlockingFunctions = Map.Map String CDeclExt
