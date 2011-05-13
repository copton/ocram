module Ocram.Test.Tests.Visitor.TranslUnit (
	tests
) where

import Ocram.Test.Tests.Visitor.Utils (runTests)
import Ocram.Test.Lib (paste)
import Ocram.Types
import Ocram.Symbols (symbol)
import Ocram.Visitor.Traverse (traverseCTranslUnit)
import Language.C.Syntax.AST 
import Ocram.Visitor.Visitor (DownVisitor, UpVisitor, crossCExtDecl)
import Data.Maybe (fromJust)

-- tests {{{1
tests = runTests "TranslUnit" [
-- test 1 {{{2
		(
			[$paste|
				void foo() { }
				void bar() { }
			|],
			getAst,
			[$paste|
				void foo() { }
				void bar() { }
			|]
		)
-- test 2 {{{2
		,(
			[$paste|
				void foo() { }
				void bar() { }
			|],
			removeBar,
			"void foo() { }"
		)
-- test 3 {{{2
		,(
			[$paste|
				void foo() { }
				void bar() { }
			|],
			doubleBar,
			[$paste|
				void foo() { }
				void bar() { }
				void bar() { }
			|]
		)
	]

-- transformations {{{1

-- test 2 {{{2
newtype DownState2 = DownState2 Symbol
type UpState = ()

instance DownVisitor DownState2

instance UpVisitor DownState2 UpState where
	crossCExtDecl (CFDefExt cfd) d@(DownState2 name) _
		| symbol cfd == name = (Just [], d, ())
		| otherwise = (Nothing, d, ())

removeBar ast = fromJust $ fst result
	where
		result :: (Maybe CTranslUnit, UpState)
		result = traverseCTranslUnit (getAst ast) $ DownState2 $ symbol "bar"

-- test 3 {{{2
newtype DownState3 = DownState3 Symbol

instance DownVisitor DownState3

instance UpVisitor DownState3 UpState where
	crossCExtDecl a@(CFDefExt cfd) d@(DownState3 name) _
		| symbol cfd == name = (Just [a, a], d, ())
		| otherwise = (Nothing, d, ())

doubleBar ast = fromJust $ fst result
	where
		result :: (Maybe CTranslUnit, UpState)
		result = traverseCTranslUnit (getAst ast) $ DownState3 $ symbol "bar"
