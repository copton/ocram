module Ocram.Transformation.ControlFlow.AddHandlerFunction (
	addHandlerFunction
) where

import Ocram.Types (Ast)
import Ocram.Symbols (symbol)
import Ocram.Analysis (CriticalFunctions, FunctionMap)
import Language.C.Data.Ident (Ident(Ident))
import Language.C.Data.Node (undefNode)
import Prelude hiding (lookup)
import Data.Map (lookup)
import Data.Set (elems)
import Data.Maybe (catMaybes)

import Language.C.Syntax.AST

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
