module Ocram.Test.Tests.Analysis.Types where

import Ocram.Test.Lib

data TestType = 
	  TTCallGraph
	| TTSanity
	| TTConstraints
	deriving (Eq, Show)

data TCase = TCase {
	  tcCode :: TCode
	, tcCallGraph :: TCallGraph
	, tcSanity :: TErrorCodes
	, tcConstraints :: TErrorCodes
	, tcExclude :: [TestType]
}
