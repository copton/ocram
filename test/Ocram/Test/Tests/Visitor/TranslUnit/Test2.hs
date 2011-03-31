module Ocram.Test.Tests.Visitor.TranslUnit.Test2 (
	test
) where

import Ocram.Types (RawAst, Ast, getAst)
import Ocram.Visitor.Visitor (DownVisitor, UpVisitor, upCFunDef, upCDecl, mapCTranslUnit)
import Ocram.Visitor.Traverse (traverseCTranslUnit)
import Ocram.Symbols (symbol, Symbol)
import Language.C.Syntax.AST (CFunDef, CExternalDeclaration(CFDefExt), CTranslUnit, CTranslationUnit(CTranslUnit))
import Data.Maybe (fromJust)
import Data.Monoid (mempty, mconcat)

type DownState = Symbol
type UpState = [CFunDef]

instance DownVisitor DownState

instance UpVisitor DownState UpState where
	upCFunDef fd fid _ 
		| symbol fd == fid = mempty
		| otherwise = [fd]

	mapCTranslUnit (CTranslUnit decls ni) _ fds = (Just (CTranslUnit decls' ni), mempty)
		where
			decls' = filter isNoFunDef decls ++ map CFDefExt (mconcat fds)
			isNoFunDef (CFDefExt _) = False
			isNoFunDef _ = True

input = [
	"void foo() { }",
	"void bar() { }"]

output = [
		"void foo()",
		"{",
		"}"
	]

transform :: RawAst -> Ast
transform raw_ast = fromJust $ fst result
	where 
		result :: (Maybe CTranslUnit, UpState)
		result = traverseCTranslUnit (getAst raw_ast) $ symbol "bar"

test = [(input, transform, output)]
