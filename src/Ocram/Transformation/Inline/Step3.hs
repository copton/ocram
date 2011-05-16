-- add thread functions
module Ocram.Transformation.Inline.Step3 
-- exports {{{1
(
	step3
) where

-- imports {{{1
import Ocram.Transformation.Inline.Names
import Ocram.Transformation.Inline.Types

import Ocram.Transformation.Util (un, ident)

import Ocram.Types
import Ocram.Visitor
import Ocram.Symbols (symbol)
import Ocram.Query (getCallChain)

import Language.C.Syntax.AST

import Data.Maybe (fromJust)
import Data.Monoid (mempty)
import qualified Data.Map as Map
import qualified Data.Set as Set

--- step3 {{{1
step3 :: CallGraph -> StartRoutines -> FunctionInfos -> Ast -> Ast
step3 cg sr fis (CTranslUnit decls ni) = CTranslUnit (decls ++ thread_functions) ni
	where
		thread_functions = map CFDefExt $ map (createThreadFunction cg fis) $ zip [1..] $ Set.elems sr

--- createThreadFunction {{{2
createThreadFunction :: CallGraph -> FunctionInfos -> (Integer, Symbol) -> CFunDef
createThreadFunction cg fis (tid, name) = 
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
		callChain = getCallChain cg name
		functions = []

-- inlineCriticalFunction {{{2
type CallChain = [Symbol]

inlineCriticalFunction :: CallChain -> FunctionInfo -> CStat
inlineCriticalFunction cc fi = fromJust $ fst result
	where
		result :: (Maybe CStat, UpState)
		result = traverseCStat (fromJust $ fiBody fi) $ DownState cc (fiVariables fi)

data DownState = DownState {
	  dCc	:: CallChain
	, dSt :: SymTab
	}

type UpState = ()

instance DownVisitor DownState

instance UpVisitor DownState UpState where
	mapCExpr (CVar iden _) d _
		| Map.member name (dSt d) = (Just $ stackAccess (dCc d) name, mempty)
		| otherwise = (Nothing, mempty)
		where name = symbol iden

	mapCExpr _ _ _ = (Nothing, mempty)

-- stackAccess {{{3
stackAccess :: CallChain -> Symbol -> CExpr
stackAccess (sr:chain) variable = foldl create base $ zip pointers members
	where
		base = CVar (ident $ stackVar sr) un
		pointers = True : cycle [False]
		members = foldr (\x l -> frameUnion : x : l) [] chain ++ [variable]
		create inner (pointer, member) = CMember inner (ident member) pointer un 
