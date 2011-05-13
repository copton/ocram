module Ocram.Test.Tests.Visitor.Decl (
	tests
) where

import Ocram.Test.Tests.Visitor.Utils (runTests)
import Ocram.Test.Lib (paste)
import Ocram.Symbols (symbol)
import Ocram.Types
import Language.C.Syntax.AST
import Language.C.Data.Node
import Language.C.Data.Ident
import Ocram.Visitor.Visitor
import Ocram.Visitor.Traverse (traverseCTranslUnit)
import Data.Monoid (mempty)
import Data.Maybe (fromJust)
import qualified Data.Set as Set

tests = runTests "Decl" [
		(
			[$paste|
				int k;
				void foo() {
					int i;
					i = 23;
					k = 42;
				}
			|],
			removeLocalVars,
			[$paste|
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
	crossCBlockItem (CBlockDecl cd) (DownState symbols) _ = (Just [], DownState $ Set.insert (symbol cd) symbols, ())
	
	crossCBlockItem _ d _ = (Nothing, d, ())

	mapCExpr (CVar var _) (DownState symbols) _
		| Set.member (symbol var) symbols = (Just (CVar (Ident "k" 0 undefNode) undefNode), mempty)
		| otherwise = (Nothing, mempty)

	mapCExpr _ _ _ = (Nothing, mempty)

removeLocalVars ast = fromJust $ fst result
	where
		result :: (Maybe CTranslUnit, UpState)
		result = traverseCTranslUnit (getAst ast) $ DownState Set.empty
