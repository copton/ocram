module Ocram.Filter.Recursion (
	checkRecursion, getErrorCodes
) where

import Ocram.Types (Result, getAst, SaneAst, CyclefreeAst(CyclefreeAst))

checkRecursion :: SaneAst -> Result CyclefreeAst
checkRecursion sane_ast = return $ CyclefreeAst $ getAst sane_ast

getErrorCodes :: SaneAst -> [Int]
getErrorCodes = undefined
