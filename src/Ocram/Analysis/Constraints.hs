module Ocram.Analysis.Constraints (
	checkConstraints
) where

import Ocram.Types (Result, AST)
import Ocram.Analysis.Types (CallGraph, StartRoutines, CriticalFunctions)
import Ocram.Symbols (Symbol)
import Ocram.Util (Location(Location))
import Ocram.Visitor (UpVisitor(..), DownVisitor(..), traverseCTranslUnit)
import Language.C.Data.Ident (Ident(Ident))
import Language.C.Syntax.AST
import Data.Monoid (mconcat)
import Data.List (intersperse)
import Data.Map (member)

checkConstraints :: AST -> CriticalFunctions -> Result AST
checkConstraints ast cf = case checkConstraints' ast cf of
	Left f -> fail $ "input violates constraints: " ++ f
	Right _ -> return ast

checkConstraints' ast cf = do
	checkFunctionPointer ast cf

checkFunctionPointer :: AST -> CriticalFunctions -> Result ()
checkFunctionPointer ast cf = case checkFunctionPointer' ast cf of
	[] -> return ()
	ls -> fail $ "taking pointer from critical function at the following locations:\n" ++ (concat $ intersperse "\n" $ map show ls)

checkFunctionPointer' :: AST -> CriticalFunctions -> UpState
checkFunctionPointer' ast cf = snd $ traverseCTranslUnit ast cf

type DownState = CriticalFunctions
instance DownVisitor DownState

type UpState = [Location]

append xs ni = Location ni : pass xs
pass xs = mconcat xs

instance UpVisitor DownState UpState where
	upCExpr (CUnary CAdrOp (CVar (Ident name _ _ ) _ ) ni) cf ls
		| name `member` cf = append ls ni 
		| otherwise = pass ls
	upCExpr _ _ ls = pass ls
