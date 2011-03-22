module Ocram.Analysis.Types.CriticalFunctions (
	CriticalFunctions
) where

import qualified Data.Set as Set
import Ocram.Analysis.Types.FunctionMap (FunctionId)

type CriticalFunctions = Set.Set FunctionId
