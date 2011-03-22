module Ocram.CreateContext (
	createContext
) where

import Ocram.Context
import Ocram.Analysis

createContext ast = ctx
	where 
		ctx = Context ast fm sr cg nAst
		fm = getFunctions ctx
		sr = findStartRoutines ctx
		cg = determineCallGraph ctx
		nAst = ast -- TODO transformation
