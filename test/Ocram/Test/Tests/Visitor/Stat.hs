module Ocram.Test.Tests.Visitor.Stat (
	tests
) where

import Ocram.Test.Tests.Visitor.Utils (runTests)
import Ocram.Test.Lib (paste)
import Ocram.Types
import Language.C.Syntax.AST
import Language.C.Syntax.Constants
import Ocram.Visitor.DefaultStates (emptyDownState, EmptyDownState)
import Ocram.Visitor.Visitor (UpVisitor, mapCStat)
import Data.Monoid (mempty)
import Ocram.Visitor.Traverse (traverseCTranslUnit)
import Data.Maybe (fromJust)

-- tests {{{1
tests = runTests "Stat" [
-- test 1 {{{2	
		(
			[$paste|
				int foo() { 
					return 0; 
				}
			|],
			zeroToOne,
			[$paste|
				int foo() {
					return 1;
				}
			|]
		)
-- test 2 {{{2
	,(
		[$paste|
			int foo() { 
				return 0x0L; 
			}
		|],
		zeroToOne,
		[$paste|
			int foo() {
					return 0x1L;
			}
		|]
		)
	]

-- transformations {{{1
-- zeroToOne {{{2

type UpState = ()

instance UpVisitor EmptyDownState UpState where
	mapCStat (CReturn (Just (CConst (CIntConst (CInteger 0 repr flags) ni1))) ni2) _ _ = 
		(Just (CReturn (Just (CConst (CIntConst (CInteger 1 repr flags) ni1))) ni2), mempty)
	
	mapCStat _ _ _ = (Nothing, mempty)

zeroToOne ast = fromJust $ fst result
	where
		result :: (Maybe CTranslUnit, UpState)
		result = traverseCTranslUnit (getAst ast) emptyDownState
