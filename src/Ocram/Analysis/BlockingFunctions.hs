module Ocram.Analysis.BlockingFunctions 
-- exports {{{1
(
	blocking_functions
) where

-- imports {{{1
import Ocram.Types
import Ocram.Names (blockingAttr)
import Data.Set as Set
import Ocram.Visitor (UpVisitor(..), EmptyDownState, emptyDownState, traverseCTranslUnit, ListVisitor)
import Language.C.Syntax.AST
import Language.C.Data.Ident (Ident(Ident))

-- blocking_functions :: Ast -> ER BlockingFunctions {{{1
blocking_functions :: Ast -> ER BlockingFunctions
blocking_functions ast = return $ snd $ traverseCTranslUnit ast emptyDownState

instance UpVisitor EmptyDownState BlockingFunctions where
	upCExtDecl o@(CDeclExt (CDecl ss [(Just (CDeclr (Just (Ident name _ _)) [CFunDeclr _ _ _] Nothing _ _), Nothing, Nothing)] _)) _ _
		| any isBlockingAttribute ss = (o, Set.singleton name)
		| otherwise = (o, Set.empty)

	upCExtDecl o _ _ = (o, Set.empty)

isBlockingAttribute (CTypeQual (CAttrQual (CAttr (Ident blockingAttr _ _) [] _))) = True
isBlockingAttribute _ = False

instance ListVisitor EmptyDownState BlockingFunctions
