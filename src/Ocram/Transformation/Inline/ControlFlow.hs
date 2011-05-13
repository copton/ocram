module Ocram.Transformation.Inline.ControlFlow 
-- exports {{{1
(
	transformControlFlow
) where

-- imports {{{1
import Ocram.Types 
import Ocram.Query (getCallChain)
import Ocram.Visitor (DownVisitor(..), UpVisitor(..), traverseCTranslUnit, traverseCFunDef)
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
transformControlFlow bf cg cf sr ast@(CTranslUnit decls ni) = CTranslUnit (decls' ++ threadFunctions) ni
	where
		decls' = foldr (processExtDecl cf) [] decls
		threadFunctions = []

processExtDecl :: CriticalFunctions -> CExtDecl -> [CExtDecl] -> [CExtDecl]
processExtDecl cf ced@(CDeclExt cd) decls
	| (symbol cd) `Set.member` cf = CDeclExt (createBlockingFunctionDeclr cd) : decls
	| otherwise = ced : decls

processExtDecl cf cefd@(CFDefExt cfd) decls
	| (symbol cfd) `Set.member` cf = decls
	| otherwise = cefd : decls

processExtDecl _ x decls = x : decls

-- createBlockingFunctionDeclr {{{2

-- createThreadFunction {{{2
createThreadFunction :: BlockingFunctions -> StartRoutines -> CallGraph -> Map.Map Symbol CFunDef -> CriticalFunctions -> (Int, Symbol) -> CFunDef
createThreadFunction bf sr cg cfs cf (tid, fName) =
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
		mapFunction chain@(fName:_) = transformCriticalFunction cf chain fName $ cfs Map.! fName

-- transformCriticalFunction {{{1
transformCriticalFunction :: CriticalFunctions -> [Symbol] -> Symbol -> CFunDef -> [CBlockItem]
transformCriticalFunction cf chain fName fd = header : extractBody fd' ++ [footer]
	where
		d = DownState' chain lVars cf
		lVars = Set.fromList $ map symbol $ extractParams fd
		extractBody (CFunDef _ _ _ (CCompound _ body _) _) = body
		(Just(fd'), _) = traverseCFunDef fd d :: (Maybe CFunDef, UpState')
		header = CBlockStmt $ createLabel fName 0
		footer = CBlockStmt $ CReturn Nothing un

data DownState' = DownState' {
		getChain :: [Symbol],
		getLocalVars :: Set.Set Symbol,
		getCf' :: CriticalFunctions
	}

type UpState' = ()

instance DownVisitor DownState' where
	downCStat (CCompound idents items ni) d =
		d { getLocalVars = Set.union (getLocalVars d) decls' }
		where   
			decls' = Set.fromList $ map (symbol . unpDecl) (filter isCBlockDecl items)

	downCStat _ d = d

instance UpVisitor DownState' UpState' where
	mapCStat (CCompound idents items ni) d _ =
		(Just (CCompound idents items' ni), mempty)
		where   
			items' = foldl (processBlockItem (getCf' d)) [] items

	mapCStat _ _ us = (Nothing, mconcat us)

	mapCExpr (CVar (Ident vName _ _) _ ) d us
		| Set.member vName (getLocalVars d) =
				(Just (stackAccess (getChain d) vName), mconcat us)
		| otherwise = (Nothing, mconcat us)

	mapCExpr (CCall (CVar (Ident fName _ _) _) params _) d _
		| Set.member fName (getCf' d) =
			(Just $ CStatExpr (CCompound [] [CBlockStmt (CExpr Nothing un)] un) un, mempty)
		| otherwise = (Nothing, mempty)

	mapCExpr _ _ _ = (Nothing, mempty)

processBlockItem :: CriticalFunctions -> [CBlockItem] -> CBlockItem -> [CBlockItem]
processBlockItem = undefined
-- processBlockItem _ items (CBlockDecl _) = items
-- processBlockItem cf items cs@(CBlockStmt (CExpr (Just (CCall (CVar (Ident fName _ _) _) params)) _)) = undefined

stackAccess :: [Symbol] -> Symbol -> CExpr
stackAccess (sr:chain) variable = foldl create base $ zip pointers members
	where
		base = CVar (ident $ stackVar sr) un
		pointers = True : cycle [False]
		members = foldr (\x l -> frameUnion : x : l) [] chain ++ [variable]
		create inner (pointer, member) = CMember inner (ident member) pointer un 

extractParams :: CFunDef -> [CDecl]
extractParams (CFunDef _ (CDeclr _ [(CFunDeclr (Right (ps, False)) _ _)] _ _ _) _ _ _) = ps

unpDecl (CBlockDecl d) = d

isCBlockDecl (CBlockDecl _) = True
isCBlockDecl _ = False

createLabel name id = CLabel (ident (label name id)) (CExpr Nothing un) [] un
