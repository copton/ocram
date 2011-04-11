module Ocram.Test.Tests.Symbol.ExtDecl (
	tests
) where

import Ocram.Symbols (symbol)
import Language.C.Syntax.AST (CTranslationUnit(CTranslUnit), CExternalDeclaration(CFDefExt, CDeclExt))
import Ocram.Test.Tests.Symbol.Util (runTests)

reduce (CTranslUnit [CFDefExt cfd] _) = symbol cfd
reduce (CTranslUnit [CDeclExt cde] _) = symbol cde

tests = runTests "ExtDecl" reduce [
		("void foo();", "foo")
	, ("void foo() { }", "foo")
	, ("struct foo { };", "foo")
	, ("union foo { };", "foo")
	, ("enum foo { A };", "foo")
	, ("enum { A, B };", "<<no_name>>")
	]
