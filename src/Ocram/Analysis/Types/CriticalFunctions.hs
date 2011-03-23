module Ocram.Analysis.Types.CriticalFunctions (
	CriticalFunctions
) where

import qualified Data.Set as Set
import Ocram.Symbols (Symbol)

type CriticalFunctions = Set.Set Symbol
