module Ocram.Test.Tests.Analysis.Types where

import Ocram.Test.Lib

data TestType = 
	  TTBlockingFunctions
	| TTStartFunctions
	| TTCriticalFunctions
	| TTCallGraph
	| TTSanity
	| TTConstraints
	deriving (Eq, Show)

data TCase = TCase {
	  tcCode :: TCode
	, tcBlockingFunctions :: TBlockingFunctions
	, tcStartFunctions :: TStartFunctions
	, tcCriticalFunctions :: TCriticalFunctions
	, tcCallGraph :: TCallGraph
	, tcSanity :: TErrorCodes
	, tcConstraints :: TErrorCodes
	, tcExclude :: [TestType]
}
