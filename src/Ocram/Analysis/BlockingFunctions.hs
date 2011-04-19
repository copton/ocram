module Ocram.Analysis.BlockingFunctions 
-- exports {{{1
(
	determineBlockingFunctions
) where

-- imports {{{1
import Ocram.Types (Result, getAst, SaneAst, BlockingFunctions, Context(getSaneAst))
import Ocram.Names (blockingAttr)
import qualified Data.Set as Set
import Ocram.Visitor (UpVisitor(..), EmptyDownState, emptyDownState, traverseCTranslUnit)
import Language.C.Syntax.AST
import Language.C.Data.Ident (Ident(Ident))

-- determineBlockingFunctions :: Context -> Result BlockingFunctions {{{1
determineBlockingFunctions :: Context -> Result BlockingFunctions
determineBlockingFunctions ctx = do
	ast <- getSaneAst ctx
	return $ snd $ traverseCTranslUnit (getAst ast) emptyDownState

instance UpVisitor EmptyDownState BlockingFunctions where
	upCExtDecl cd@(CDeclExt (CDecl ss [(Just (CDeclr (Just (Ident name _ _)) [CFunDeclr _ _ _] Nothing _ _), Nothing, Nothing)] _)) _ _ =
		if any isBlockingAttribute ss then 
			Set.singleton name 
		else 
			Set.empty
	upCExtDecl _ _ _ = Set.empty

isBlockingAttribute (CTypeQual (CAttrQual (CAttr (Ident blockingAttr _ _) [] _))) = True
isBlockingAttribute _ = False
