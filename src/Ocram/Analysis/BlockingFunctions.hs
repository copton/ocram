module Ocram.Analysis.BlockingFunctions (
	determineBlockingFunctions
) where

import Ocram.Analysis.Types (BlockingFunctions)
import Ocram.Types (AST, Result)
import qualified Data.Map as Map
import Ocram.Visitor (UpVisitor(..), EmptyDownState, emptyDownState, traverseCTranslUnit)
import Language.C.Syntax.AST
import Language.C.Data.Ident (Ident(Ident))

determineBlockingFunctions :: AST -> Result BlockingFunctions
determineBlockingFunctions ast = return $ snd $ traverseCTranslUnit ast emptyDownState

instance UpVisitor EmptyDownState BlockingFunctions where
	upCExtDecl cd@(CDeclExt (CDecl ss [(Just (CDeclr (Just (Ident name _ _)) [CFunDeclr _ _ _] Nothing _ _), Nothing, Nothing)] _)) _ _ =
		if any isBlockingAttribute ss then 
			Map.singleton name cd 
		else 
			Map.empty
	upCExtDecl _ _ _ = Map.empty

isBlockingAttribute (CTypeQual (CAttrQual (CAttr (Ident "tc_blocking" _ _) [] _))) = True
isBlockingAttribute _ = False
