module Ocram.Analysis.StartRoutines (
	findStartRoutines
) where

import Ocram.Types (FunctionMap, StartRoutines, Result)
import Language.C.Syntax.AST
import Language.C.Data.Ident (Ident(Ident))
import Data.Map (elems)
import Ocram.Symbols (symbol)

findStartRoutines :: FunctionMap -> Result StartRoutines
findStartRoutines fm = return $ map symbol $ filter isStartRoutine $ elems fm

isStartRoutine :: CFunDef -> Bool
isStartRoutine (CFunDef specs _ _ _ _) = any checkAttr specs 

checkAttr (CTypeQual (CAttrQual (CAttr (Ident "tc_run_thread" _ _) [] _))) = True
checkAttr _ = False
