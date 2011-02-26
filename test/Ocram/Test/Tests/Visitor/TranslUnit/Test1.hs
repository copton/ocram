module Ocram.Test.Tests.Visitor.TranslUnit.Test1 (
	test
) where

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

translate = id

test = [(input, translate, output)]
