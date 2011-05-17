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

import Data.Maybe (fromJust, isJust)
import Data.Monoid (mempty)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List

--- step3 {{{1
step3 :: CallGraph -> StartRoutines -> BlockingFunctions -> FunctionInfos -> Ast -> Ast
step3 cg sr bf fis (CTranslUnit decls ni) = CTranslUnit (decls ++ thread_functions) ni
	where
		thread_functions = map CFDefExt $ map (createThreadFunction cg bf fis) $ zip [1..] $ Set.elems sr

--- createThreadFunction {{{2
createThreadFunction :: CallGraph -> BlockingFunctions -> FunctionInfos -> (Integer, Symbol) -> CFunDef
createThreadFunction cg bf fis (tid, name) = 
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
		callChain = filter (not . (flip Set.member bf)) $ getCallChain cg name
		functions = concatMap mapFunction $ List.drop 1 $ List.inits callChain
		mapFunction chain@(fName:_) = inlineCriticalFunction chain fis fName

-- inlineCriticalFunction {{{2
type CallChain = [Symbol]

inlineCriticalFunction :: CallChain -> FunctionInfos -> Symbol -> [CBlockItem]
inlineCriticalFunction cc fis fName = lbl : body' ++ close : []
	where
		result :: (Maybe CStat, UpState)
		result = traverseCStat body $ DownState cc (fiVariables fi) fis fName 1
		fi = fis Map.! fName
		body = fromJust $ fiBody fi
		body' = extractBody $ fromJust $ fst result
		extractBody (CCompound _ body _) = body
		lbl = createLabel fName 0
		close = CBlockStmt $ CReturn Nothing un

data DownState = DownState {
	  dCc	:: CallChain
	, dSt :: SymTab
	, dFis :: FunctionInfos
	, dFName :: Symbol
	, dLbl :: Int
	}

type UpState = ()

instance DownVisitor DownState

instance UpVisitor DownState UpState where
	-- rewrite access to local variables
	mapCExpr (CVar iden _) d _
		| Map.member name (dSt d) = (Just $ stackAccess (dCc d) (Just name), mempty)
		| otherwise = (Nothing, mempty)
		where name = symbol iden

	mapCExpr _ _ _ = (Nothing, mempty)

	-- rewrite critical function calls
	crossCBlockItem (CBlockStmt (CExpr (Just (CCall (CVar iden _) params _)) _)) d _
		| Map.member name (dFis d) = (Just $ criticalFunctionCall d name params, d {dLbl = (dLbl d) + 1})
		| otherwise = (Nothing, d)
		where
			name = symbol iden

-- criticalFunctionCall {{{3
criticalFunctionCall :: DownState -> Symbol -> [CExpr] -> [CBlockItem]
criticalFunctionCall d cfName params = parameters ++ continuation : call : return : lbl : []
	where
		lid = dLbl d
		chain' = (dCc d) ++ [cfName]
		fi = (dFis d) Map.! cfName

		parameters = map (createParamAssign chain') $ zip params $ fiParams fi

		continuation = CBlockStmt (CExpr (Just (CAssign CAssignOp (stackAccess chain' (Just contVar)) (CUnary CAdrOp (CVar (ident $ label (dFName d) lid) un) un) un)) un)

		call = CBlockStmt (CExpr (Just (CCall (CVar (ident cfName) un) [CUnary CAdrOp (stackAccess chain' Nothing) un] un)) un)

		return = CBlockStmt (CReturn Nothing un)

		lbl = createLabel (dFName d) lid

-- createParamAssign
createParamAssign :: CallChain -> (CExpr, CDecl) -> CBlockItem
createParamAssign chain (exp, decl) = CBlockStmt (CExpr (Just (CAssign CAssignOp lhs rhs un)) un)
	where
		lhs = stackAccess chain (Just $ symbol decl)
		rhs = exp

-- stackAccess {{{3
stackAccess :: CallChain -> Maybe Symbol -> CExpr
stackAccess (sr:chain) variable = foldl create base $ zip pointers members
	where
		variables = if isJust variable then [fromJust variable] else []
		base = CVar (ident $ stackVar sr) un
		pointers = True : cycle [False]
		members = foldr (\x l -> frameUnion : x : l) [] chain ++ variables
		create inner (pointer, member) = CMember inner (ident member) pointer un 

-- createLabel {{{3
createLabel name id = CBlockStmt $ CLabel (ident (label name id)) (CExpr Nothing un) [] un
