module Ocram.Test.Tests.Symbol
-- exports {{{1
(
	tests
) where

-- imports {{{1
import Language.C.Syntax.AST (CTranslationUnit(CTranslUnit), CExternalDeclaration(CFDefExt, CDeclExt))
import Ocram.Symbols (symbol, Symbol)
import Ocram.Test.Lib (parse, enumTestGroup)
import Ocram.Types (Ast)
import Test.HUnit ((@=?))

-- tests {{{1
tests = enumTestGroup "Symbol" $ map runTest [
		("void foo();", "foo")
	, ("void foo() { }", "foo")
	, ("struct foo { };", "foo")
	, ("union foo { };", "foo")
	, ("enum foo { A };", "foo")
	, ("enum { A, B };", "<<no_name>>")
	]

reduce (CTranslUnit [CFDefExt cfd] _) = symbol cfd
reduce (CTranslUnit [CDeclExt cde] _) = symbol cde

runTest (code, expected) = expected @=? reduce (parse code)
