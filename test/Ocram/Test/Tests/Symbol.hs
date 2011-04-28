module Ocram.Test.Tests.Symbol (
	tests
) where

import Ocram.Symbols (symbol)
import Language.C.Syntax.AST (CTranslationUnit(CTranslUnit), CExternalDeclaration(CFDefExt, CDeclExt))
import Test.HUnit (Test(TestLabel,TestCase,TestList), assertEqual)
import Ocram.Types (getAst, Ast, Symbol)
import Ocram.Test.Lib (parse')

reduce (CTranslUnit [CFDefExt cfd] _) = symbol cfd
reduce (CTranslUnit [CDeclExt cde] _) = symbol cde

tests = runTests "Symbol" reduce [
		("void foo();", "foo")
	, ("void foo() { }", "foo")
	, ("struct foo { };", "foo")
	, ("union foo { };", "foo")
	, ("enum foo { A };", "foo")
	, ("enum { A, B };", "<<no_name>>")
	]

runTests :: String -> (Ast -> String) -> [(String, Symbol)] -> Test
runTests label reduce tests = TestLabel label $ TestList $ map (runTest reduce) $ zip [1..] tests

runTest :: (Ast -> String) -> (Int, (String, String)) -> Test
runTest reduce (number, (code, expected)) = TestCase $ assertEqual name expected symbol
	where
		name = "test" ++ show number
		symbol = reduce $ getAst $ parse' code
