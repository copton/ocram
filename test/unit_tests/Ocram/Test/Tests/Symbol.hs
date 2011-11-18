module Ocram.Test.Tests.Symbol
-- exports {{{1
(
	tests
) where

-- imports {{{1
import Language.C.Syntax.AST (CTranslationUnit(CTranslUnit), CExternalDeclaration(CFDefExt, CDeclExt))
import Ocram.Symbols (symbol, Symbol)
import Ocram.Test.Lib (parse)
import Ocram.Types (Ast)
import Test.HUnit (Test(TestLabel,TestCase,TestList), assertEqual)

-- tests {{{1
tests = runTests "Symbol" reduce [
		("void foo();", "foo")
	, ("void foo() { }", "foo")
	, ("struct foo { };", "foo")
	, ("union foo { };", "foo")
	, ("enum foo { A };", "foo")
	, ("enum { A, B };", "<<no_name>>")
	]

reduce (CTranslUnit [CFDefExt cfd] _) = symbol cfd
reduce (CTranslUnit [CDeclExt cde] _) = symbol cde

runTests :: String -> (Ast -> String) -> [(String, Symbol)] -> Test
runTests label reduce tests = TestLabel label $ TestList $ map (runTest reduce) $ zip [1..] tests

runTest :: (Ast -> String) -> (Int, (String, String)) -> Test
runTest reduce (number, (code, expected)) = TestCase $ assertEqual name expected symbol
	where
		name = "test" ++ show number
		symbol = reduce $ parse code
