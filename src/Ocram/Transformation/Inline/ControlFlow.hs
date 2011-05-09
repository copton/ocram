module Ocram.Transformation.Inline.ControlFlow 
-- exports {{{1
(
	transformControlFlow
) where

-- imports {{{1
import Ocram.Types 
import Ocram.Query (getCallChain)
import Ocram.Visitor (DownVisitor, UpVisitor(..), traverseCTranslUnit)
import Ocram.Symbols (symbol)
import Ocram.Transformation.Util (un, ident)
import Ocram.Transformation.Inline.Names (frameType, frameParam, label, handlerFunction, contVar, frameUnion, stackVar)
import Ocram.Query (getFunDefs)

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as List
import Data.Maybe (fromJust)
import Data.Maybe (catMaybes)
import Data.Monoid
import Data.Set (member)
import Prelude hiding (lookup)

import Language.C.Syntax.AST
import Language.C.Data.Ident (Ident(Ident))

-- transformControlFlow :: CallGraph -> CriticalFunctions -> StartRoutines -> Ast -> Ast {{{1
transformControlFlow :: BlockingFunctions -> CallGraph -> CriticalFunctions -> StartRoutines -> Ast -> Ast
transformControlFlow bf cg cf sr ast = 
	let
		emptyDownState = DownState cg cf sr bf
		(ast', _) = (traverseCTranslUnit ast emptyDownState) :: (Maybe CTranslUnit, UpState)
	in
		fromJust ast'
	

-- types {{{2
data DownState = DownState {
	getCg :: CallGraph,
	getCf :: CriticalFunctions,
	getSr :: StartRoutines,
	getBf :: BlockingFunctions
	}

instance DownVisitor DownState 

data UpState = UpState {
	getRest :: [CExtDecl],
	getBfDeclrs :: [CDecl],
	getCfBodies :: Map.Map Symbol CStat
	}

instance Monoid UpState where
	mempty = UpState mempty mempty mempty
	(UpState a b c) `mappend` (UpState a' b' c') = UpState (a `mappend` a') (b `mappend` b') (c `mappend` c')

instance UpVisitor DownState UpState where
	mapCTranslUnit (CTranslUnit _ ni) (DownState cg _ sr bf) us = (Just ctu, mempty)
		where
			ctu = CTranslUnit (rest ++ blockingFunctions ++ threadFunctions) ni
			rest = concatMap getRest us
			blockingFunctions = map (CDeclExt . createBlockingFunctionDeclr) $ concatMap getBfDeclrs us
			threadFunctions = map (CFDefExt . ctf) $ zip [1..] $ Set.elems sr
			bodies = mconcat $ map getCfBodies us
			ctf = createThreadFunction bf sr cg bodies

	upCDecl cd (DownState _ cf _ _) _
		| (symbol cd) `member` cf = UpState mempty [cd] mempty
		| otherwise = UpState [CDeclExt cd] mempty mempty

	upCFunDef cfd (DownState _ cf _ _) _
		| (symbol cfd) `member` cf = UpState mempty mempty $ Map.singleton (symbol cfd) (extractBody cfd)
		| otherwise = UpState [CFDefExt cfd] mempty mempty
		where
			extractBody (CFunDef _ _ _ body _) = body

-- createBlockingFunctionDeclr :: CDecl -> CDecl {{{2
createBlockingFunctionDeclr :: CDecl -> CDecl
createBlockingFunctionDeclr cd = let fName = symbol cd in
   CDecl [CTypeSpec (CVoidType un)] [(Just (CDeclr (Just (ident fName)) [CFunDeclr (Right ([CDecl [CTypeSpec (CTypeDef (ident (frameType fName)) un)] [(Just (CDeclr (Just (ident frameParam)) [CPtrDeclr [] un] Nothing [] un), Nothing, Nothing)] un], False)) [] un] Nothing [] un), Nothing, Nothing)] un

-- createThreadFunction :: BlockingFunctions -> StartRoutines -> CallGraph -> Map.Map Symbol CStat -> (Int, Symbol) -> CFunDef {{{2
createThreadFunction :: BlockingFunctions -> StartRoutines -> CallGraph -> Map.Map Symbol CStat -> (Int, Symbol) -> CFunDef
createThreadFunction bf sr cg bodies (tid, fName) =
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
		[] (CCompound [] (intro : functions) un) un
	where
		intro = CBlockStmt (CIf (CBinary CNeqOp (CVar (ident contVar) un) (CVar (ident "null") un) un) (CGotoPtr (CVar (ident contVar) un) un) Nothing un)
		callChain = filter (not . (flip Set.member bf)) $ getCallChain cg fName
		functions = concatMap mapFunction $ List.drop 1 $ List.inits callChain
		mapFunction chain@(fName:rest) = inlineFunction $ transformCriticalFunction fName (bodies Map.! fName) chain

inlineFunction (CCompound _ block _) = block

-- transformCriticalFunction :: Symbol -> CStat -> [Symbol] -> CStat {{{2
transformCriticalFunction :: Symbol -> CStat -> [Symbol] -> CStat
transformCriticalFunction fName body chain = CCompound [] [CBlockStmt (CExpr (Just $ stackAccess (chain ++ ["hemmerned"]) "foo" ) un)] un

stackAccess :: [Symbol] -> Symbol -> CExpr
stackAccess (sr:chain) variable = foldl create base $ zip pointers members
	where
		base = CVar (ident $ stackVar sr) un
		pointers = True : cycle [False]
		members = concatMap (\x -> [frameUnion, x]) chain ++ [variable]
		create :: CExpr -> (Bool, Symbol) -> CExpr
		create inner (pointer, member) = CMember inner (ident member) pointer un 

-- createLabel (name, body) = 
--   (CBlockStmt (CLabel (Ident (label name 0) 0 un) (CExpr Nothing un) [] un)) : body
