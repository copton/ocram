module Ocram.Transformation.Inline.ControlFlow 
-- exports {{{1
(
	transformControlFlow
) where

-- imports {{{1
import Ocram.Types 
import Ocram.Visitor (DownVisitor, UpVisitor(..), traverseCTranslUnit)
import Ocram.Symbols (symbol)
import Ocram.Transformation.Util (un, ident)
import Ocram.Transformation.Inline.Names (label, handlerFunction, contVar)
import Ocram.Query (getFunDefs)

import Data.Set (elems)
import Data.Maybe (fromJust)
import Data.Maybe (catMaybes)
import Data.Monoid (mconcat)
import Data.Set (member)
import Prelude hiding (lookup)
import qualified Data.Map as Map

import Language.C.Syntax.AST
import Language.C.Data.Ident (Ident(Ident))

-- transformControlFlow :: Ast -> CriticalFunctions -> DefinedFunctions -> Ast {{{1
transformControlFlow :: CriticalFunctions -> DefinedFunctions -> Ast -> Ast
transformControlFlow cf df ast = ast
	-- let fm = getFunDefs ast df in
	-- addHandlerFunction cf fm $ removeCriticalFunctions cf ast

addHandlerFunction :: CriticalFunctions -> Map.Map Symbol CFunDef -> Int -> Ast -> Ast
addHandlerFunction cf fm tid (CTranslUnit decls ni) = (CTranslUnit (handlerFun fm cf tid : decls) ni)
	
handlerFun fm cf = (CFDefExt
	(CFunDef [CTypeSpec (CVoidType un)]
		(CDeclr (Just (ident handlerFunction))
			[CFunDeclr
					(Right
						 ([CDecl [CTypeSpec (CVoidType un)]
								 [(Just (CDeclr (Just (ident contVar)) 
										[CPtrDeclr [] un] Nothing [] un), Nothing, Nothing)]
								 un],
							False))
					[] un]
			 Nothing [] un)
		[] (CCompound [] (bodies fm cf) un) un))

bodies fm cf = concatMap (createLabel . extractBody) $ catMaybes $ map (flip Map.lookup fm) $ elems cf

extractBody fd@(CFunDef _ _ _ (CCompound _ body _) _) = (symbol fd, body)

createLabel (name, body) = 
	(CBlockStmt (CLabel (Ident (label name 0) 0 un) (CExpr Nothing un) [] un)) : body



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
