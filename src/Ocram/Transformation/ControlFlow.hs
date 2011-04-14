module Ocram.Transformation.ControlFlow (
	transformControlFlow
) where

import Ocram.Types(Ast)
import Ocram.Transformation.Types (FunctionInfos)
import Ocram.Analysis (CriticalFunctions, FunctionMap)
import Ocram.Types (Result, StacklessAst, OutputAst(OutputAst), getAst)
import Ocram.Visitor (DownVisitor, UpVisitor(..), traverseCTranslUnit)
import Ocram.Symbols (symbol)

import Data.Set (elems)
import Data.Maybe (fromJust)
import Data.Maybe (catMaybes)
import Data.Monoid (mconcat)
import Data.Set (member)
import Prelude hiding (lookup)
import Data.Map (lookup)

import Language.C.Syntax.AST
import Language.C.Data.Node (undefNode)
import Language.C.Data.Ident (Ident(Ident))


transformControlFlow :: StacklessAst -> FunctionInfos -> CriticalFunctions -> FunctionMap -> Result OutputAst
transformControlFlow ast fi cf fm = return $ OutputAst $ addHandlerFunction cf fm $ removeCriticalFunctions cf $ getAst ast


addHandlerFunction :: CriticalFunctions -> FunctionMap -> Ast -> Ast
addHandlerFunction cf fm (CTranslUnit decls ni) = (CTranslUnit (handlerFun fm cf : decls) ni)
	
handlerFun fm cf = (CFDefExt
	(CFunDef [CTypeSpec (CVoidType undefNode)]
		(CDeclr (Just (ident "ec_events"))
			[CFunDeclr
					(Right
						 ([CDecl [CTypeSpec (CTypeDef (ident "ec_continuation_t") undefNode)]
								 [(Just (CDeclr (Just (ident "ec_cont")) 
										[CPtrDeclr [] undefNode] Nothing [] undefNode), Nothing, Nothing)]
								 undefNode],
							False))
					[] undefNode]
			 Nothing [] undefNode)
		[] (CCompound [] (bodies fm cf) undefNode) undefNode))

bodies fm cf = concatMap (label . extractBody) $ catMaybes $ map (`lookup`fm) $ elems cf

extractBody fd@(CFunDef _ _ _ (CCompound _ body _) _) = (symbol fd, body)

label (name, body) = 
	(CBlockStmt (CLabel (Ident ("ec_" ++ name) 0 undefNode) (CExpr Nothing undefNode) [] undefNode)) : body

ident s = Ident s 0 undefNode



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
