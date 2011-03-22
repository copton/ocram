module Ocram.CreateContext (
	createContext
) where

import Ocram.Context (Context(Context))
import Language.C.Syntax.AST (CTranslUnit)

import Ocram.Analysis (getFunctions, findStartRoutines, determineCallGraph, determineBlockingFunctions, determineCriticalFunctions)
import Ocram.Transformation (tc2ec)

createContext :: CTranslUnit -> Context
createContext ia = ctx
	where 
		ctx = Context ia fm sr cg bf cf oa
		fm = getFunctions ctx
		sr = findStartRoutines ctx
		cg = determineCallGraph ctx
		bf = determineBlockingFunctions ctx
		cf = determineCriticalFunctions ctx
		oa = tc2ec ctx
