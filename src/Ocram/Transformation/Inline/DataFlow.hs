module Ocram.Transformation.Inline.DataFlow 
-- exports {{{1
(
	transformDataFlow
) where

-- imports {{{1
import Ocram.Transformation.Inline.Names (contType, contVar, resVar, frameType, frameUnion, frameVar)
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
import Data.Maybe (fromJust, catMaybes)
import Language.C.Syntax.AST
import Language.C.Data.Ident (Ident(Ident))

-- transformDataFlow :: CallGraph -> CriticalFunctions -> BlockingFunctions -> Ast -> Ast {{{1
transformDataFlow :: CallGraph -> CriticalFunctions -> BlockingFunctions -> Ast -> Ast
transformDataFlow cg cf bf ast = 
	let emptyDownState = DownState cg cf bf Nothing Set.empty in
	let (ast', _) = (traverseCTranslUnit ast emptyDownState) :: (Maybe CTranslUnit, UpState) in
	fromJust ast'

-- types {{{2
data DownState = DownState {
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
	downCExtDecl (CFDefExt fd) d@(DownState _ cf _ _ _) =
		let name = (symbol fd) in
		if Set.member name cf 
			then d { getCfName = Just name, getLocalVars = localVars }
			else d
		where
			localVars = Set.fromList $ map symbol $ extractParams' fd

	downCExtDecl (CDeclExt cd) d@(DownState _ _ bf _ _) =
		let name = (symbol cd) in
		if Set.member name bf
			then d { getCfName = Just name }
			else d
		
	downCExtDecl _ d = d

	downCStat (CCompound idents items ni) d@(DownState _ _ _ (Just name) decls) =
		d { getLocalVars = Set.union decls decls' }
		where
			decls' = Set.fromList $ map (symbol.unpDecl) (filter isCBlockDecl items) 

	downCStat _ d = d

instance UpVisitor DownState UpState where
	mapCTranslUnit (CTranslUnit decls ni) (DownState cg _ bf _ _) us = (Just ctu, mempty)
		where
			ctu = CTranslUnit (frames ++ decls) ni
			frames = createTStackFrames bf cg fis
			fis = Map.fromList $ catMaybes $ map getFunctionInfo us

	upCExtDecl (CDeclExt cd) (DownState _ _ _ (Just name) _) _ = 
		UpState [] $ Just (name, decl2fi cd)

	upCExtDecl (CFDefExt fd) (DownState _ _ _ (Just name) _) us =
		UpState [] $ Just (name, def2fi fd variables)
		where
			variables = concatMap getVariables' us

	upCExtDecl _ _ _ = mempty

	mapCStat (CCompound idents items ni) (DownState _ _ _ (Just name) _) us = 
		(Just (CCompound idents items' ni), upState)
		where
			(decls, items') = partition isCBlockDecl items
			upState = UpState (varDecls ++ concatMap getVariables' us) Nothing
			varDecls = map unpDecl decls

	mapCStat _ _ us = (Nothing, mconcat us)

	mapCExpr (CVar (Ident vName _ _)_ ) (DownState _ _ _ (Just fName) localVars) us
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

-- createTStackFrames :: BlockingFunctions -> CallGraph -> FunctionInfos -> [CExtDecl] {{{2
createTStackFrames :: BlockingFunctions -> CallGraph -> FunctionInfos -> [CExtDecl]
createTStackFrames bf cg fi = concatMap (topologicSort cg fi) $ Set.elems bf

topologicSort :: CallGraph -> FunctionInfos -> Symbol -> [CExtDecl]
topologicSort cg fi sym = myFrame : otherFrames
	where
		myFrame = createTStackFrame cg (sym, fi Map.! sym)
		otherFrames = concatMap (topologicSort cg fi) $ Set.elems $ cgCallers $ cg Map.! sym

createTStackFrame :: CallGraph -> (Symbol, FunctionInfo) -> CExtDecl
createTStackFrame cg (name, fi@(FunctionInfo resultType params)) = 
	 CDeclExt
		 (CDecl
				[CStorageSpec (CTypedef un),
				 CTypeSpec
					 (CSUType
							(CStruct CStructTag Nothing
								 (Just (
										(CDecl [CTypeSpec (CTypeDef (ident contType) un)]
											 [(Just (CDeclr (Just (ident contVar)) [] Nothing [] un), Nothing,
												 Nothing)]
											 un) : result ?: nestedFrames ?: params))
								 [] un)
							un)]

				[(Just (CDeclr (Just (ident (frameType name))) [] Nothing [] un), Nothing, Nothing)]
				un)
	where
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
