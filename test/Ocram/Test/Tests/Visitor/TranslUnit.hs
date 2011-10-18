module Ocram.Test.Tests.Visitor.TranslUnit
-- exports {{{1
(
	tests
) where

-- imports {{{1
import Data.Maybe (fromJust)
import Data.Monoid
import Language.C.Syntax.AST 
import Language.C.Syntax.Constants
import Ocram.Symbols (symbol, Symbol)
import Ocram.Test.Lib (paste)
import Ocram.Test.Tests.Visitor.Utils (runTests)
import Ocram.Transformation.Util (ident, un)
import Ocram.Visitor.Traverse (traverseCTranslUnit)
import Ocram.Visitor.Visitor

-- tests {{{1
tests = runTests "TranslUnit" [
-- test 1 {{{2
		(
			[paste|
				void foo() { }
				void bar() { }
			|],
			id,
			[paste|
				void foo() { }
				void bar() { }
			|]
		)
-- test 2 {{{2
		,(
			[paste|
				void foo() { }
				void bar() { }
			|],
			removeBar,
			"void foo() { }"
		)
-- test 3 {{{2
		,(
			[paste|
				void foo() { }
				void bar() { }
			|],
			doubleBar,
			[paste|
				void foo() { }
				void bar() { }
				void bar() { }
			|]
		)
-- test 4 {{{2
		,(
			[paste|
				void foo() { }
				void bar() { }
			|],
			countFunctions,
			[paste|
				int count=2;
			|]
		)
	]

-- transformations {{{1

-- test 2 {{{2
newtype DownState2 = DownState2 Symbol
type UpState = ()

instance DownVisitor DownState2

instance UpVisitor DownState2 UpState 

instance ListVisitor DownState2 UpState where
	nextCExtDecl o@(CFDefExt cfd) d@(DownState2 name) u
		| symbol cfd == name = ([], d, u)
		| otherwise = ([o], d, u)

removeBar ast = fst result
	where
		result :: (CTranslUnit, UpState)
		result = traverseCTranslUnit ast $ DownState2 $ symbol "bar"

-- test 3 {{{2
newtype DownState3 = DownState3 Symbol

instance DownVisitor DownState3

instance UpVisitor DownState3 UpState

instance ListVisitor DownState3 UpState where
	nextCExtDecl o@(CFDefExt cfd) d@(DownState3 name) u
		| symbol cfd == name = ([o, o], d, u)
		| otherwise = ([o], d, u)

doubleBar ast = fst result
	where
		result :: (CTranslUnit, UpState)
		result = traverseCTranslUnit ast $ DownState3 $ symbol "bar"

-- test 4 {{{2
newtype DownState4 = DownState4 ()

instance DownVisitor DownState4

newtype UpState4 = UpState4 Integer

instance Monoid UpState4 where
	mempty = UpState4 0
	mappend (UpState4 x) (UpState4 x') = UpState4 (x + x')

instance UpVisitor DownState4 UpState4 where
	upCFunDef o _ _ = (o, UpState4 1)

instance ListVisitor DownState4 UpState4

countFunctions ast = wrap $ snd $ traverseCTranslUnit ast $ DownState4 ()

wrap (UpState4 count) = 
  (CTranslUnit
     [CDeclExt
        (CDecl [CTypeSpec (CIntType un)]
           [(Just (CDeclr (Just (ident "count")) [] Nothing [] un),
             Just (CInitExpr (CConst (CIntConst (cInteger count) un)) un), Nothing)]
           un)]
     un)
