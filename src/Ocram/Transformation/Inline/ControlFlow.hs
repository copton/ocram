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
import Ocram.Transformation.Inline.Names (frameType, frameParam, label, handlerFunction, contVar)
import Ocram.Query (getFunDefs)

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Maybe (catMaybes)
import Data.Monoid
import Data.Set (member)
import Prelude hiding (lookup)

import Language.C.Syntax.AST
import Language.C.Data.Ident (Ident(Ident))

-- transformControlFlow :: CallGraph -> CriticalFunctions -> StartRoutines -> Ast -> Ast {{{1
transformControlFlow :: CallGraph -> CriticalFunctions -> StartRoutines -> Ast -> Ast
transformControlFlow cg cf sr ast = 
	let
		emptyDownState = DownState cg cf sr
		(ast', _) = (traverseCTranslUnit ast emptyDownState) :: (Maybe CTranslUnit, UpState)
	in
		fromJust ast'
	

-- types {{{2
data DownState = DownState {
	getCg :: CallGraph,
	getCf :: CriticalFunctions,
	getSr :: StartRoutines
	}

instance DownVisitor DownState 

data UpState = UpState {
	getRest :: [CExtDecl],
	getBfDeclrs :: [CDecl],
	getCfBodies :: Map.Map Symbol [CBlockItem]
	}

instance Monoid UpState where
	mempty = UpState mempty mempty mempty
	(UpState a b c) `mappend` (UpState a' b' c') = UpState (a `mappend` a') (b `mappend` b') (c `mappend` c')

instance UpVisitor DownState UpState where
	mapCTranslUnit (CTranslUnit _ ni) (DownState cg _ sr) us = (Just ctu, mempty)
		where
			ctu = CTranslUnit (rest ++ blockingFunctions ++ threadFunctions) ni
			rest = concatMap getRest us
			blockingFunctions = map (CDeclExt . createBlockingFunctionDeclr) $ concatMap getBfDeclrs us
			threadFunctions = map (CFDefExt . ctf) $ zip [1..] $ Set.elems sr
			ctf = createThreadFunction cg $ mconcat $ map getCfBodies us

	upCDecl cd (DownState _ cf _) _
		| (symbol cd) `member` cf = UpState mempty [cd] mempty
		| otherwise = UpState [CDeclExt cd] mempty mempty

	upCFunDef cfd (DownState _ cf _) _
		| (symbol cfd) `member` cf = UpState mempty mempty $ Map.singleton (symbol cfd) (extractBody cfd)
		| otherwise = UpState [CFDefExt cfd] mempty mempty
		where
			extractBody (CFunDef _ _ _ (CCompound _ body _) _) = body

-- createBlockingFunctionDeclr :: CDecl -> CDecl {{{1
createBlockingFunctionDeclr :: CDecl -> CDecl
createBlockingFunctionDeclr cd = let fName = symbol cd in
   CDecl [CTypeSpec (CVoidType un)] [(Just (CDeclr (Just (ident fName)) [CFunDeclr (Right ([CDecl [CTypeSpec (CTypeDef (ident (frameType fName)) un)] [(Just (CDeclr (Just (ident frameParam)) [CPtrDeclr [] un] Nothing [] un), Nothing, Nothing)] un], False)) [] un] Nothing [] un), Nothing, Nothing)] un

-- createThreadFunction :: CallGraph -> Map.Map Symbol [CBlockItem] -> (Int, Symbol) -> CFunDef {{{1
createThreadFunction :: CallGraph -> Map.Map Symbol [CBlockItem] -> (Int, Symbol) -> CFunDef
createThreadFunction cg bodies (tid, fName) = 
	CFunDef [CTypeSpec (CVoidType un)]
		(CDeclr (Just (ident (handlerFunction tid)))
			[CFunDeclr
					(Right
						 ([CDecl [CTypeSpec (CVoidType un)]
								 [(Just (CDeclr (Just (ident contVar)) 
										[CPtrDeclr [] un] Nothing [] un), Nothing, Nothing)]
								 un],
							False))
					[] un]
			 Nothing [] un)
		[] (CCompound [] (intro : []) un) un
	where
		intro = CBlockStmt (CIf (CBinary CNeqOp (CVar (ident contVar) un) (CVar (ident "null") un) un) (CGotoPtr (CVar (ident contVar) un) un) Nothing un)

-- addHandlerFunction :: CriticalFunctions -> Map.Map Symbol CFunDef -> Int -> Ast -> Ast
-- addHandlerFunction cf fm tid (CTranslUnit decls ni) = (CTranslUnit (handlerFun fm cf tid : decls) ni)
--   

-- bodies fm cf = concatMap (createLabel . extractBody) $ catMaybes $ map (flip Map.lookup fm) $ elems cf


-- createLabel (name, body) = 
--   (CBlockStmt (CLabel (Ident (label name 0) 0 un) (CExpr Nothing un) [] un)) : body
