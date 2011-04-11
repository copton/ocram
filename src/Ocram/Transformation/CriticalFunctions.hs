module Ocram.Transformation.CriticalFunctions (transformCriticalFunctions) where

import Ocram.Types (Result, ValidAst(ValidAst), getAst, Ast)
import Ocram.Analysis (CriticalFunctions, FunctionMap)
import Ocram.Visitor (DownVisitor, UpVisitor(..), traverseCTranslUnit)
import Ocram.Symbols (symbol, Symbol)
import Prelude hiding (lookup)
import Data.Map (member, keys, (!), lookup)
import Data.Maybe (fromJust)
import Data.Monoid (mconcat)
import Language.C.Syntax.AST
import Language.C.Data.Node (undefNode)
import Language.C.Data.Ident (Ident(Ident))

import Debug.Trace (trace)

transformCriticalFunctions :: ValidAst -> CriticalFunctions -> FunctionMap -> Result ValidAst
transformCriticalFunctions valid_ast cf fm = return $ ValidAst $ createHandlerFunction (removeCriticalFunctions ast cf) cf fm
	where
		ast = getAst valid_ast

removeCriticalFunctions :: Ast -> CriticalFunctions -> Ast
removeCriticalFunctions ast cf = fromJust $ fst $ (traverseCTranslUnit ast (DownState cf) :: (Maybe CTranslUnit, UpState))

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



createHandlerFunction :: Ast -> CriticalFunctions -> FunctionMap -> Ast
createHandlerFunction (CTranslUnit decls ni) cf fm = (CTranslUnit (handlerFun : decls) ni)
	where
		handlerFun = (CFDefExt
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
				[] (CCompound [] bodies undefNode) undefNode))
		bodies = concatMap process (keys cf)
		process k = case lookup k fm of
			(Just fd) -> extractBody fd 
			Nothing -> []
		extractBody (CFunDef _ _ _ (CCompound _ body _) _) = body

ident s = Ident s 0 undefNode
