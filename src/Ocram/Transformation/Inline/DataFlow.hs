module Ocram.Transformation.Inline.DataFlow 
-- exports {{{1
(
	transformDataFlow
) where

-- imports {{{1
import Ocram.Transformation.Inline.Names (stackVar, contVar, resVar, frameType, frameUnion)
import Ocram.Transformation.Inline.Util (tStackAccess)
import Ocram.Transformation.Util (un, ident)
import Ocram.Types 
import Ocram.Symbols (symbol)
import Ocram.Query (getFunDefs, getFunDecls)
import Ocram.Util ((?:))
import Data.Monoid (Monoid, mappend, mempty, mconcat)
import Ocram.Visitor
import Control.Exception (assert)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (partition)
import Data.Maybe (fromJust, catMaybes, isJust)
import Language.C.Syntax.AST
import Language.C.Data.Ident (Ident(Ident))

-- transformDataFlow :: StartRoutines -> CallGraph -> CriticalFunctions -> BlockingFunctions -> Ast -> Ast {{{1
transformDataFlow :: StartRoutines -> CallGraph -> CriticalFunctions -> BlockingFunctions -> Ast -> Ast
transformDataFlow sr cg cf bf ast = 
	let 
		emptyDownState = DownState sr cg cf bf Nothing Set.empty
		(ast', _) = (traverseCTranslUnit ast emptyDownState) :: (Maybe CTranslUnit, UpState) 
	in
		fromJust ast'

-- types {{{2
data DownState = DownState {
	getSr :: StartRoutines,
	getCg :: CallGraph,
	getCf :: CriticalFunctions,
	getBf :: BlockingFunctions,
	getCfName :: Maybe Symbol,
	getLocalVars :: Set.Set Symbol
	}

data FunctionInfo = FunctionInfo {
		getResultType :: CTypeSpec,
		getVariables :: [CDecl]
	}

type FunctionInfos = Map.Map Symbol FunctionInfo
data UpState = UpState {
	getVariables' :: [CDecl],
	getFunctionInfo :: Maybe (Symbol, FunctionInfo)
	}

instance Monoid UpState where
	mempty = UpState [] Nothing
	(UpState p Nothing) `mappend` (UpState p' Nothing) = UpState (p `mappend` p') Nothing

-- visitor {{{2
instance DownVisitor DownState where
	downCExtDecl (CFDefExt fd) d =
		let name = (symbol fd) in
		if Set.member name $ getCf d
			then d { getCfName = Just name, getLocalVars = localVars }
			else d
		where
			localVars = Set.fromList $ map symbol $ extractParams' fd

	downCExtDecl (CDeclExt cd) d =
		let name = (symbol cd) in
		if Set.member name $ getBf d
			then d { getCfName = Just name }
			else d
		
	downCExtDecl _ d = d

	downCStat (CCompound idents items ni) d
		| isJust $ getCfName d = d { getLocalVars = Set.union (getLocalVars d) decls' }
		| otherwise = d
		where
			decls' = Set.fromList $ map (symbol.unpDecl) (filter isCBlockDecl items) 

	downCStat _ d = d

instance UpVisitor DownState UpState where
	mapCTranslUnit (CTranslUnit decls ni) (DownState sr cg _ bf _ _) us = (Just ctu, mempty)
		where
			ctu = CTranslUnit (frames ++ stacks ++ decls) ni
			frames = createTStackFrames sr bf cg fis
			fis = Map.fromList $ catMaybes $ map getFunctionInfo us
			stacks = map createTStack $ zip [1..] $ Set.elems sr
	
	upCExtDecl (CDeclExt cd) (DownState _ _ _ _ (Just name) _) _ = 
		UpState [] $ Just (name, decl2fi cd)

	upCExtDecl (CFDefExt fd) (DownState _ _ _ _ (Just name) _) us =
		UpState [] $ Just (name, def2fi fd variables)
		where
			variables = concatMap getVariables' us

	upCExtDecl _ _ _ = mempty

	mapCStat (CCompound idents items ni) (DownState _ _ _ _ (Just name) _) us = 
		(Just (CCompound idents items' ni), upState)
		where
			(decls, items') = partition isCBlockDecl items
			upState = UpState (varDecls ++ concatMap getVariables' us) Nothing
			varDecls = map unpDecl decls

	mapCStat _ _ us = (Nothing, mconcat us)

	mapCExpr (CVar (Ident vName _ _)_ ) (DownState _ _ _ _ (Just fName) localVars) us
		| Set.member vName localVars = (Just (tStackAccess fName vName), mconcat us)
		| otherwise = (Nothing, mconcat us)

	mapCExpr _ _ us = (Nothing, mconcat us)

decl2fi :: CDecl -> FunctionInfo
decl2fi (CDecl tss [(Just (CDeclr _ [cfd] _ _ _), Nothing, Nothing)] _) =
	FunctionInfo (extractTypeSpec tss) (extractParams cfd)

def2fi :: CFunDef -> [CDecl] -> FunctionInfo
def2fi fd@(CFunDef tss _ _ _ _) variables = 
	FunctionInfo resultType (params ++ variables)
	where
		params = extractParams' fd
		resultType = extractTypeSpec tss

extractParams' :: CFunDef -> [CDecl]
extractParams' (CFunDef _ (CDeclr _ [cfd] _ _ _) [] _ _) = extractParams cfd

extractParams :: CDerivedDeclr -> [CDecl]
extractParams (CFunDeclr (Right (ps, False)) _ _) = ps

extractTypeSpec :: [CDeclSpec] -> CTypeSpec
extractTypeSpec [] = error "assertion failed: type specifier expected"
extractTypeSpec ((CTypeSpec ts):xs) = ts
extractTypeSpec (_:xs) = assert (not $ null xs) (extractTypeSpec xs)

isCBlockDecl (CBlockDecl _) = True
isCBlockDecl _ = False

unpDecl (CBlockDecl d) = d

-- createTStack :: (Int, Symbol) -> CExtDecl {{{1
createTStack :: (Int, Symbol) -> CExtDecl
createTStack (tid, fName) = CDeclExt (CDecl [CTypeSpec (CTypeDef (ident (frameType fName)) un)] [(Just (CDeclr (Just (ident (stackVar tid))) [] Nothing [] un), Nothing, Nothing)] un)

-- createTStackFrames :: StartRoutines -> BlockingFunctions -> CallGraph -> FunctionInfos -> [CExtDecl] {{{1
createTStackFrames :: StartRoutines -> BlockingFunctions -> CallGraph -> FunctionInfos -> [CExtDecl]
createTStackFrames sr bf cg fi = concatMap (topologicSort sr cg fi) $ Set.elems bf

topologicSort :: StartRoutines -> CallGraph -> FunctionInfos -> Symbol -> [CExtDecl]
topologicSort sr cg fi sym = myFrame : otherFrames
	where
		myFrame = createTStackFrame sr cg (sym, fi Map.! sym)
		otherFrames = concatMap (topologicSort sr cg fi) $ Set.elems $ cgCallers $ cg Map.! sym

createTStackFrame :: StartRoutines -> CallGraph -> (Symbol, FunctionInfo) -> CExtDecl
createTStackFrame sr cg (name, fi@(FunctionInfo resultType params)) = 
	 CDeclExt
		 (CDecl
				[CStorageSpec (CTypedef un),
				 CTypeSpec
					 (CSUType
							(CStruct CStructTag Nothing
								 (Just (
										 continuation ?: result ?: nestedFrames ?: params))
								 [] un)
							un)]

				[(Just (CDeclr (Just (ident (frameType name))) [] Nothing [] un), Nothing, Nothing)]
				un)
	where
		continuation
			| Set.member name sr = Nothing
			| otherwise = Just (CDecl [CTypeSpec (CVoidType un)] [(Just (CDeclr (Just (ident contVar)) [CPtrDeclr [] un] Nothing [] un), Nothing, Nothing)] un)

		result = case resultType of
			(CVoidType _) -> Nothing
			_ -> Just $ CDecl [CTypeSpec resultType] [(Just (CDeclr (Just (ident resVar)) [] Nothing [] un), Nothing, Nothing)] un

		nestedFrames = createNestedFramesUnion cg (name, fi)

createNestedFramesUnion :: CallGraph -> (Symbol, FunctionInfo) -> Maybe CDecl
createNestedFramesUnion cg (name, fi) = result
	where
		result = if null entries then Nothing else Just createDecl
		entries = map createEntry $ Set.elems $ cgCallees $ cg Map.! name
		createEntry sym = CDecl [CTypeSpec (CTypeDef (ident (frameType sym)) un)]
											[(Just (CDeclr (Just (ident sym)) [] Nothing [] un), Nothing, Nothing)] un 
		createDecl = CDecl [CTypeSpec (CSUType (CStruct CUnionTag Nothing (Just entries) [] un) un)] [(Just (CDeclr (Just (ident frameUnion)) [] Nothing [] un), Nothing, Nothing)] un
