module Ocram.Test.Tests.Visitor.TranslUnit.Test1 (
	test
) where

import Ocram.Types (getAst, RawAst, Ast)

input = [
	"void foo() { }",
	"void bar() { }"]

output = [
		"void foo()",
		"{",
		"}",
		"void bar()",
		"{",
		"}"
	]

transform :: RawAst -> Ast
transform raw_ast = getAst raw_ast

test = [(input, transform, output)]
