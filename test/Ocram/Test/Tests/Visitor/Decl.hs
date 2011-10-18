module Ocram.Test.Tests.Visitor.Decl
-- exports {{{1
(
	tests
) where

-- imports {{{1
import Ocram.Test.Tests.Visitor.Utils (runTests)
import Ocram.Test.Lib (paste)
import Ocram.Symbols (symbol, Symbol)
import Language.C.Syntax.AST
import Language.C.Data.Node
import Language.C.Data.Ident
import Ocram.Visitor.Visitor
import Ocram.Visitor.Traverse (traverseCTranslUnit)
import Data.Monoid (mempty)
import Data.Maybe (fromJust)
import qualified Data.Set as Set

-- tests {{{1
tests = runTests "Decl" [
		(
			[paste|
				int k;
				void foo() {
					int i;
					i = 23;
					k = 42;
				}
			|],
			removeLocalVars,
			[paste|
				int k;
				void foo() {
					k = 23;
					k = 42;
				}
			|]
		)				
	]

newtype DownState = DownState (Set.Set Symbol)
type UpState = ()

instance DownVisitor DownState

instance UpVisitor DownState UpState where
	upCExpr o@(CVar var _) d@(DownState symbols) u
		| Set.member (symbol var) symbols = (CVar (Ident "k" 0 undefNode) undefNode, mempty)
		| otherwise = (o, u)

	upCExpr o d u = (o, u)

instance ListVisitor DownState UpState where
	nextCBlockItem (CBlockDecl cd) (DownState symbols) u = ([], DownState $ Set.insert (symbol cd) symbols, u)

	nextCBlockItem o d u = ([o], d, u)


removeLocalVars ast = fst result
	where
		result :: (CTranslUnit, UpState)
		result = traverseCTranslUnit ast $ DownState Set.empty
