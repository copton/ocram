module Ocram.Analysis.BlockingFunctions (
	determineBlockingFunctions
) where

import Ocram.Analysis.Types (BlockingFunctions)
import Language.C.Data.Ident (Ident(Ident))
import Data.Map as Map
import Ocram.Visitor (UpVisitor(..), EmptyDownState, emptyDownState, traverseCTranslUnit)
import Ocram.Context (Context(Context, ctxInputAst))
import Language.C.Syntax.AST

instance UpVisitor EmptyDownState BlockingFunctions where
	upCExtDecl cd@(CDeclExt (CDecl ss [(Just (CDeclr (Just (Ident name _ _)) [CFunDeclr _ _ _] Nothing _ _), Nothing, Nothing)] _)) _ _ =
		if any isBlockingAttribute ss then 
			Map.singleton name cd 
		else 
			Map.empty
	upCExtDecl _ _ _ = Map.empty

isBlockingAttribute (CTypeQual (CAttrQual (CAttr (Ident "tc_blocking" _ _) [] _))) = True
isBlockingAttribute _ = False

determineBlockingFunctions :: Context -> BlockingFunctions
determineBlockingFunctions ctx = snd $ traverseCTranslUnit (ctxInputAst ctx) emptyDownState
