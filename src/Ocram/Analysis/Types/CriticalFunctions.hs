module Ocram.Analysis.Types.CriticalFunctions (
	CriticalFunctions, Signature(Signature)
) where

import qualified Data.Map as Map
import Ocram.Symbols (Symbol)
import Language.C.Syntax.AST (CTypeSpec)

data Signature = Signature CTypeSpec [(CTypeSpec, Symbol)]

-- map of all critical function signatures
type CriticalFunctions = Map.Map Symbol Signature
