module Ocram.Test.Tests.Analysis.Types where

import Ocram.Options (Options)

type TCode = String
type TBlockingFunctions = [String]
type TDefinedFunctions = [String]
type TCallGraph = [(String, [String], [String])]
type TStartRoutines = [String]
type TCriticalFunctions = [String]
type TErrorCodes = [Int]

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
	tcOptions :: Options,
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
