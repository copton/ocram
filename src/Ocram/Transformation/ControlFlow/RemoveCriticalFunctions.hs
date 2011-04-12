module Ocram.Transformation.ControlFlow.RemoveCriticalFunctions (
	removeCriticalFunctions
) where

import Ocram.Types(Ast)
import Ocram.Analysis (CriticalFunctions)
import Data.Maybe (fromJust)
import Ocram.Visitor (DownVisitor, UpVisitor(..), traverseCTranslUnit)
import Language.C.Syntax.AST (CExtDecl, CExternalDeclaration(CDeclExt, CFDefExt), CTranslUnit, CTranslationUnit(CTranslUnit))
import Data.Monoid (mconcat)
import Ocram.Symbols (symbol)
import Data.Set (member)

removeCriticalFunctions :: CriticalFunctions -> Ast -> Ast
removeCriticalFunctions cf ast = fromJust $ fst $ (traverseCTranslUnit ast (DownState cf) :: (Maybe CTranslUnit, UpState))

newtype DownState = DownState CriticalFunctions
instance DownVisitor DownState 

type UpState = [CExtDecl]
instance UpVisitor DownState UpState where
	mapCTranslUnit (CTranslUnit _ ni) _ fds = (Just (CTranslUnit (mconcat fds) ni), [])

	upCDecl cd (DownState cf) _
		| (symbol cd) `member` cf = []
		| otherwise = [CDeclExt cd]

	upCFunDef cfd (DownState cf) _
		| (symbol cfd) `member` cf = []
		| otherwise = [CFDefExt cfd]
