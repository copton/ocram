module Ocram.Analysis.StartRoutines 
-- exports {{{1
(
	findStartRoutines
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

-- findStartRoutines :: Context -> Result StartRoutines {{{1
findStartRoutines :: Context -> Result StartRoutines
findStartRoutines ctx = do
	ast <- getSaneAst ctx
	df <- getDefinedFunctions ctx
	let df' = getFunDefs ast df
	return $ fromList $ map symbol $ filter isStartRoutine $ elems df'

isStartRoutine :: CFunDef -> Bool
isStartRoutine (CFunDef specs _ _ _ _) = any checkAttr specs 

checkAttr (CTypeQual (CAttrQual (CAttr (Ident startRoutineAttr _ _) [] _))) = True
checkAttr _ = False
