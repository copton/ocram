module Ocram.Transformation.CriticalFunctions (transformCriticalFunctions) where

import Ocram.Types (Result, ValidAst(ValidAst), getAst, Ast)
import Ocram.Analysis (CriticalFunctions)
import Ocram.Visitor (DownVisitor, UpVisitor(..), traverseCTranslUnit)
import Ocram.Symbols (symbol)
import Data.Map (member)
import Data.Maybe (fromJust)
import Data.Monoid (mconcat)
import Language.C.Syntax.AST

newtype DownState = DownState CriticalFunctions
instance DownVisitor DownState 

type UpState = [CExtDecl]
instance UpVisitor DownState UpState where
	mapCTranslUnit (CTranslUnit _ ni) _ fds = (Just (CTranslUnit (mconcat fds) ni), [])

	upCDecl cd _ _ = [CDeclExt cd]

	upCFunDef cfd (DownState cf) _
		| (symbol cfd) `member` cf = []
		| otherwise = [CFDefExt cfd]

removeCriticalFunctions :: Ast -> CriticalFunctions -> Ast
removeCriticalFunctions ast cf = fromJust $ fst $ (traverseCTranslUnit ast (DownState cf) :: (Maybe CTranslUnit, UpState))


transformCriticalFunctions :: ValidAst -> CriticalFunctions -> Result ValidAst
transformCriticalFunctions ast cf = return $ ValidAst $ removeCriticalFunctions (getAst ast) cf
