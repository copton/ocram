module Ocram.Analysis.Constraints (
	check_constraints
) where

import Ocram.Analysis.Filter
import Ocram.Types
import Ocram.Visitor (UpVisitor(..), DownVisitor(..), traverseCTranslUnit, ListVisitor)
import Language.C.Data.Ident (Ident(Ident))
import Language.C.Syntax.AST
import qualified Data.Set as Set
import Data.Monoid (mconcat)

-- check_constraints ::  CriticalFunctions -> StartRoutines -> Ast -> ER () {{{1
check_constraints ::  CriticalFunctions -> StartRoutines -> Ast -> ER ()
check_constraints cf sr ast = performFilter (descriptor cf sr) ast

-- utils {{{1
printError :: Int -> String
printError 1 = "taking pointer from critical function"
printError 2 = "at least one thread must be started"
printError x = error $ "unknown error code: " ++ show x

checker :: CriticalFunctions -> StartRoutines -> Ast -> [Error Int]
checker cf sr ast = checkFunctionPointer cf ast ++ checkThreads ast sr

descriptor cf sr = Filter "constraint" (checker cf sr) printError id

checkThreads (CTranslUnit _ ni) sr 
	| Set.null sr = [Error 2 ni]
	| otherwise = []

checkFunctionPointer :: CriticalFunctions -> Ast -> [Error Int]
checkFunctionPointer cf ast = snd $ traverseCTranslUnit ast $ DownState cf

newtype DownState = DownState CriticalFunctions
instance DownVisitor DownState

append es code location = Error code location : es

type UpState = [Error Int]
instance UpVisitor DownState UpState where
	upCExpr o@(CUnary CAdrOp (CVar (Ident name _ _ ) _ ) ni) (DownState cf) u
		| name `Set.member` cf = (o, append u 1 ni)
		| otherwise = (o, u)
	upCExpr o _ u = (o, u)

instance ListVisitor DownState UpState
