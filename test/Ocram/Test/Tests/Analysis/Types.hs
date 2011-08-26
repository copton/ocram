module Ocram.Test.Tests.Analysis.Types where

import Ocram.Test.Lib

data TestType = 
	  TTDefinedFunctions
	| TTBlockingFunctions
	| TTStartRoutines
	| TTCriticalFunctions
	| TTCallGraph
	| TTSanity
	| TTADG
	| TTConstraints
	deriving (Eq, Show)

data TCase = TCase {
	tcCode :: TCode,
	tcDefinedFunctions :: TDefinedFunctions,
	tcBlockingFunctions :: TBlockingFunctions,
	tcStartRoutines :: TStartRoutines,
	tcCriticalFunctions :: TCriticalFunctions,
	tcCallGraph :: TCallGraph,
	tcSanity :: TErrorCodes,
	tcADG :: TErrorCodes,
	tcConstraints :: TErrorCodes,
	tcExclude :: [TestType]
}	
