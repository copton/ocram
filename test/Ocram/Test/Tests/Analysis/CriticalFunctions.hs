module Ocram.Test.Tests.Analysis.CriticalFunctions (
	tests
) where

import Ocram.Test.Tests.Analysis.Utils (runTests)
import Ocram.Analysis.Types (Signature(Signature))
import Ocram.Context (ctxCriticalFunctions)
import Data.Map (toList)
import Data.List (sort)
import Language.C.Syntax.AST
import Language.C (pretty)
import Ocram.Symbols (Symbol)

data F = F String String [(String, String)]
instance Eq F where
	(F n r ps) == (F n' r' ps') = n == n' && r == r' && (sort ps) == (sort ps')
instance Show F where
	show (F n r ps) = show r ++ " " ++ show n ++ "(" ++ show ps ++ ")"
instance Ord F where
	(F name _ _) <= (F name' _ _) = name <= name'

data L = L [F]
instance Eq L where
	(L fs) == (L fs') = (sort fs) == (sort fs')
instance Show L where
	show (L fs) = show fs

procTypeSpec :: CTypeSpec -> String
procTypeSpec ts = show $ pretty ts

procParam :: (CTypeSpec, Symbol) -> (String, String)
procParam (ts, symbol) = (procTypeSpec ts, symbol)

procFunction :: (Symbol, Signature) -> F
procFunction (symbol, (Signature r ps)) = F symbol (procTypeSpec r) (map procParam ps)

reduce = L.(map procFunction).toList.ctxCriticalFunctions

tests = runTests "CriticalFunctions" reduce [
	 ("void foo() { }", L [])
	,("", L [])
	,("void foo();", L [])
	,("__attribute__((tc_blocking)) void foo();", L [F "foo" "void" []])
	,("__attribute__((tc_blocking)) void foo(); void bar() { foo(); }", L [F "foo" "void" [], F "bar" "void" []])
	,("__attribute__((tc_blocking)) void foo(); void bar(); void baz { bar(); foo(); }", L [F "baz" "void" [], F "foo" "void" []])
	,("__attribute__((tc_blocking)) void D(); void B() {D();} void C() {D();} void A() {B();C();}", L [
		F "A" "void" [],
		F "B" "void" [],
		F "C" "void" [],
		F "D" "void" []])
	]
