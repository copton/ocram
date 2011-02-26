{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Ocram.Test.Tests.Visitor.Stat.Test1 (
	test
) where

import Ocram.Visitor.DefaultStates (emptyDownState, EmptyDownState)
import Ocram.Visitor.Visitor (UpVisitor, mapCStat)
import Ocram.Visitor.Traverse (traverseCTranslUnit)
import Data.Monoid (mempty)
import Language.C.Syntax.AST (CStatement(CReturn), CExpression(CConst), CConstant(CIntConst), CTranslUnit)
import Language.C.Syntax.Constants (CInteger(CInteger))
import Data.Maybe (fromJust)

type UpState = ()

instance UpVisitor EmptyDownState UpState where
	mapCStat (CReturn (Just (CConst (CIntConst (CInteger 0 repr flags) ni1))) ni2) _ _ = 
		(Just (CReturn (Just (CConst (CIntConst (CInteger 1 repr flags) ni1))) ni2), ())
	
	mapCStat _ _ _ = (Nothing, mempty)

input1 = ["int foo() { return 0; }"]

output1 = [
		"int foo()",
		"{",
		"    return 1;",
		"}"
	]

input2 = ["int foo() { return 0x0L; }"]

output2 = [
		"int foo()",
		"{",
		"    return 0x1L;",
		"}"
	]

transform :: CTranslUnit -> CTranslUnit
transform ctu = fromJust $ fst result
	where
		result :: (Maybe CTranslUnit, UpState)
		result = traverseCTranslUnit ctu emptyDownState

test = [(input1, transform, output1), (input2, transform, output2)]
