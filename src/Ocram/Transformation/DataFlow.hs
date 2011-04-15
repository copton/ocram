module Ocram.Transformation.DataFlow (
	transformDataFlow
) where

import Ocram.Types 
import Ocram.Util ((?:))
import Data.Monoid (Monoid, mappend, mempty, mconcat)
import Ocram.Visitor (traverseCFunDef, emptyDownState, EmptyDownState, UpVisitor(mapCStat))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (partition)
import Data.Maybe (fromJust)
import Language.C.Data.Node (undefNode)
import Language.C.Data.Ident (Ident(Ident))
import Language.C.Syntax.AST

transformDataFlow :: ValidAst -> CallGraph -> CriticalFunctions -> BlockingFunctions -> FunctionMap -> Result (FunctionInfos, StacklessAst)
transformDataFlow valid_ast cg cf bf fm = return (fi, StacklessAst $ stackless_ast ast)
	where
		ast = getAst valid_ast
		stackless_ast (CTranslUnit decls ni) = CTranslUnit (decls ++ frames) ni
		frames = createTStackFrames bf cg fi
		fi = retrieveFunctionInfos cf bf fm

-- retrieveFunctionInfos
retrieveFunctionInfos :: CriticalFunctions -> BlockingFunctions -> FunctionMap -> FunctionInfos
retrieveFunctionInfos cf bf fm = Map.union blockingFunctions recBlockingFunctions
	where
		blockingFunctions = Map.mapWithKey retrieveFunctionInfo' bf
		recBlockingFunctions = Map.mapWithKey retrieveFunctionInfo $ Map.filterWithKey (\k _ -> Set.member k cf) fm

retrieveFunctionInfo' :: Symbol -> CExtDecl -> FunctionInfo
retrieveFunctionInfo' symbol (CDeclExt (CDecl tss [(Just (CDeclr _ [cfd] Nothing [] _), Nothing, Nothing)] _)) =
	FunctionInfo symbol resultType params []
	where
		params = extractParams cfd
		resultType = extractTypeSpec tss

retrieveFunctionInfo :: Symbol -> CFunDef -> FunctionInfo
retrieveFunctionInfo symbol fd@(CFunDef tss (CDeclr _ [cfd] Nothing _ _) [] _ _) = 
	FunctionInfo symbol resultType (params ++ variables) body
	where
		(variables, body) = process fd
		params = extractParams cfd
		resultType = extractTypeSpec tss

process :: CFunDef -> (Frame, [CBlockItem])
process fd = (frame, body)
	where
		(maybeFd, frame) = traverseCFunDef fd emptyDownState
		body = extractBody $ fromJust maybeFd

type UpState = Frame

instance UpVisitor EmptyDownState UpState where
	mapCStat (CCompound idents items ni) _ us = (Just (CCompound idents items' ni), upState)
		where
			(decls, items') = partition isCBlockDecl items
			upState = map (\(CBlockDecl d) -> d) decls ++ mconcat us

	mapCStat _ _ us = (Nothing, mconcat us)
		 
isCBlockDecl (CBlockDecl _) = True
isCBlockDecl _ = False

extractBody :: CFunDef -> [CBlockItem]
extractBody (CFunDef _ _ _ (CCompound _ items _) _) = items

extractParams :: CDerivedDeclr -> [CDecl]
extractParams (CFunDeclr (Right (ps, False)) _ _) = ps

extractTypeSpec :: [CDeclSpec] -> CTypeSpec
extractTypeSpec [] = error "ill-formed input: type specifier expected"
extractTypeSpec ((CTypeSpec ts):xs) = ts
extractTypeSpec (_:xs) = extractTypeSpec xs

-- createTStackFrames
createTStackFrames :: BlockingFunctions -> CallGraph -> FunctionInfos -> [TStackFrame]
createTStackFrames bf cg fi = concatMap (topologicSort cg fi) $ Map.keys bf

topologicSort :: CallGraph -> FunctionInfos -> Symbol -> [TStackFrame]
topologicSort cg fi sym = myFrame : otherFrames
	where
		myFrame = createTStackFrame cg $ fi Map.! sym
		otherFrames = concatMap (topologicSort cg fi) $ Set.elems $ cgCallers $ cg Map.! sym

createTStackFrame :: CallGraph -> FunctionInfo -> TStackFrame
createTStackFrame cg fi@(FunctionInfo name resultType frame _) = 
   CDeclExt
     (CDecl
        [CStorageSpec (CTypedef undefNode),
         CTypeSpec
           (CSUType
              (CStruct CStructTag Nothing
                 (Just (
                    (CDecl [CTypeSpec (CTypeDef (ident "ec_continuation_t") undefNode)]
                       [(Just (CDeclr (Just (ident "ec_cont")) [] Nothing [] undefNode), Nothing,
                         Nothing)]
                       undefNode) : result ?: nestedFrames ?: frame))
                 [] undefNode)
              undefNode)]

        [(Just (CDeclr (Just (frameIdent name)) [] Nothing [] undefNode), Nothing, Nothing)]
        undefNode)
	where
		result = case resultType of
			(CVoidType _) -> Nothing
			_ -> Just $ CDecl [CTypeSpec resultType] [(Just (CDeclr (Just (ident "ec_result")) [] Nothing [] undefNode), Nothing, Nothing)] undefNode
		nestedFrames = createNestedFramesUnion cg fi
                       
createNestedFramesUnion :: CallGraph -> FunctionInfo -> Maybe CDecl
createNestedFramesUnion cg fi = result
	where
		result = if null entries then Nothing else Just createDecl
		entries = map createEntry $ Set.elems $ cgCallees $ (Map.!) cg $ getFunctionName fi
		createEntry sym = CDecl [CTypeSpec (CTypeDef (frameIdent sym) undefNode)]
											[(Just (CDeclr (Just (ident sym)) [] Nothing [] undefNode), Nothing, Nothing)] undefNode 
		createDecl = CDecl [CTypeSpec (CSUType (CStruct CUnionTag Nothing (Just entries) [] undefNode) undefNode)] [(Just (CDeclr (Just (ident "ec_frames")) [] Nothing [] undefNode), Nothing, Nothing)] undefNode

ident s = Ident s 0 undefNode
frameIdent funcName = ident $ frameName funcName
frameName funcName = "ec_frame_" ++ funcName ++ "_t"
