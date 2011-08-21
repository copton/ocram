module Ocram.Analysis.StartRoutines 
-- exports {{{1
(
	start_routines
) where

-- imports {{{1
import Ocram.Types
import Ocram.Query (getFunDefs)
import Ocram.Names (startRoutineAttr)
import Language.C.Syntax.AST
import Language.C.Data.Ident (Ident(Ident))
import Data.Map (elems)
import Ocram.Symbols (symbol)
import Data.Set (fromList)

-- start_routines :: DefinedFunctions -> Ast -> StartRoutines {{{1
start_routines :: DefinedFunctions -> Ast -> StartRoutines
start_routines df ast = 
	let df' = getFunDefs ast df in
	fromList $ map symbol $ filter isStartRoutine $ elems df'

isStartRoutine :: CFunDef -> Bool
isStartRoutine (CFunDef specs _ _ _ _) = any checkAttr specs 

checkAttr (CTypeQual (CAttrQual (CAttr (Ident startRoutineAttr _ _) [] _))) = True
checkAttr _ = False
