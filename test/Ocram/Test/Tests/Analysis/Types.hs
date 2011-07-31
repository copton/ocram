module Ocram.Test.Tests.Analysis.Types where

import Ocram.Options (Options)

type TCode = String
type TBlockingFunctions = [String]
type TDefinedFunctions = [String]
type TCallGraph = [(String, [String], [String])]
type TStartRoutines = [String]
type TCriticalFunctions = [String]
type TErrorCodes = [Int]

data TCase = TCase {
	tcCode :: TCode,
	tcOptions :: Options,
	tcADG :: TErrorCodes,
	tcBlockingFunctions :: TBlockingFunctions,
	tcCallGraph :: TCallGraph,
	tcConstraints :: TErrorCodes,
	tcCriticalFunctions :: TCriticalFunctions,
	tcDefinedFunctions :: TDefinedFunctions,
	tcSanity :: TErrorCodes,
	tcStartRoutines :: TStartRoutines
}	
