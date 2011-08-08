module Ocram.Test.Tests.Visitor.Stat (
	tests
) where

import Ocram.Test.Tests.Visitor.Utils (runTests)
import Ocram.Test.Lib (paste)
import Ocram.Types
import Language.C.Syntax.AST
import Language.C.Syntax.Constants
import Ocram.Visitor.DefaultStates (emptyDownState, EmptyDownState)
import Ocram.Visitor.Visitor (UpVisitor, upCStat, ListVisitor)
import Data.Monoid (mempty)
import Ocram.Visitor.Traverse (traverseCTranslUnit)
import Data.Maybe (fromJust)

-- tests {{{1
tests = runTests "Stat" [
-- test 1 {{{2	
		(
			[paste|
				int foo() { 
					return 0; 
				}
			|],
			zeroToOne,
			[paste|
				int foo() {
					return 1;
				}
			|]
		)
-- test 2 {{{2
	,(
		[paste|
			int foo() { 
				return 0x0L; 
			}
		|],
		zeroToOne,
		[paste|
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
	upCStat (CReturn (Just (CConst (CIntConst (CInteger 0 repr flags) ni1))) ni2) _ _ = 
		(CReturn (Just (CConst (CIntConst (CInteger 1 repr flags) ni1))) ni2, mempty)
	
	upCStat o _ _ = (o, mempty)

instance ListVisitor EmptyDownState UpState

zeroToOne ast = fst result
	where
		result :: (CTranslUnit, UpState)
		result = traverseCTranslUnit ast emptyDownState
