module Ocram.CreateContext (
	createContext
) where

import Ocram.Context (Context(Context))
import Language.C.Syntax.AST (CTranslUnit)

import Ocram.Analysis (getFunctions, findStartRoutines, determineCallGraph)
import Ocram.Transformation (tc2ec)

createContext :: CTranslUnit -> Context
createContext ia = ctx
	where 
		ctx = Context ia fm sr cg oa
		fm = getFunctions ctx
		sr = findStartRoutines ctx
		cg = determineCallGraph ctx
		oa = tc2ec ctx
