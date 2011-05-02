module Ocram.Transformation.Inline.ControlFlow 
-- exports {{{1
(
	transformControlFlow
) where

-- imports {{{1
import Ocram.Types 
import Ocram.Visitor (DownVisitor, UpVisitor(..), traverseCTranslUnit)
import Ocram.Symbols (symbol)
import Ocram.Transformation.Inline.Names (label, handlerFunction, contType, contVar)
import Ocram.Query (getFunDefs)

import Data.Set (elems)
import Data.Maybe (fromJust)
import Data.Maybe (catMaybes)
import Data.Monoid (mconcat)
import Data.Set (member)
import Prelude hiding (lookup)
import qualified Data.Map as Map

import Language.C.Syntax.AST
import Language.C.Data.Node (undefNode)
import Language.C.Data.Ident (Ident(Ident))

-- transformControlFlow :: Ast -> CriticalFunctions -> DefinedFunctions -> Ast {{{1
transformControlFlow :: CriticalFunctions -> DefinedFunctions -> Ast -> Ast
transformControlFlow cf df ast = ast
	-- let fm = getFunDefs ast df in
	-- addHandlerFunction cf fm $ removeCriticalFunctions cf ast

addHandlerFunction :: CriticalFunctions -> Map.Map Symbol CFunDef -> Ast -> Ast
addHandlerFunction cf fm (CTranslUnit decls ni) = (CTranslUnit (handlerFun fm cf : decls) ni)
	
handlerFun fm cf = (CFDefExt
	(CFunDef [CTypeSpec (CVoidType undefNode)]
		(CDeclr (Just (ident handlerFunction))
			[CFunDeclr
					(Right
						 ([CDecl [CTypeSpec (CTypeDef (ident contType) undefNode)]
								 [(Just (CDeclr (Just (ident contVar)) 
										[CPtrDeclr [] undefNode] Nothing [] undefNode), Nothing, Nothing)]
								 undefNode],
							False))
					[] undefNode]
			 Nothing [] undefNode)
		[] (CCompound [] (bodies fm cf) undefNode) undefNode))

bodies fm cf = concatMap (createLabel . extractBody) $ catMaybes $ map (flip Map.lookup fm) $ elems cf

extractBody fd@(CFunDef _ _ _ (CCompound _ body _) _) = (symbol fd, body)

createLabel (name, body) = 
	(CBlockStmt (CLabel (Ident (label name 0) 0 undefNode) (CExpr Nothing undefNode) [] undefNode)) : body

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
