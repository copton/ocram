module Ocram.Transformation.DataFlow (
	transformDataFlow
) where

import Ocram.Types (ValidAst, Result, StacklessAst)
import Ocram.Analysis (CriticalFunctions, FunctionMap)
import Ocram.Transformation.Types (FunctionInfos, TStackFrame)
import Ocram.Symbols (Symbol)
import Ocram.Analysis (FunctionMap, CriticalFunctions)
import Ocram.Transformation.Types (FunctionInfos, FunctionInfo(FunctionInfo), Frame)
import Data.Monoid (Monoid, mappend, mempty, mconcat)
import Ocram.Visitor (traverseCFunDef, emptyDownState, EmptyDownState, UpVisitor(mapCStat))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (partition)
import Data.Maybe (fromJust)
import Language.C.Data.Node (undefNode)
import Language.C.Data.Ident (Ident(Ident))
import Language.C.Syntax.AST

transformDataFlow :: ValidAst -> CriticalFunctions -> FunctionMap -> Result (FunctionInfos, StacklessAst)
transformDataFlow valid_ast cf fm = undefined

-- retrieveFunctionInfos
retrieveFunctionInfos :: FunctionMap -> CriticalFunctions -> FunctionInfos
retrieveFunctionInfos fm cf = Map.mapWithKey retrieveFunctionInfo $ Map.filterWithKey (\k _ -> Set.member k cf) fm

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
isCBlockDecl _ = True

extractBody :: CFunDef -> [CBlockItem]
extractBody (CFunDef _ _ _ (CCompound _ items _) _) = items

extractParams :: CDerivedDeclr -> [CDecl]
extractParams (CFunDeclr (Right (ps, False)) _ _) = ps

extractTypeSpec :: [CDeclSpec] -> CTypeSpec
extractTypeSpec [] = error "ill-formed input: type specifier expected"
extractTypeSpec ((CTypeSpec ts):xs) = ts
extractTypeSpec (_:xs) = extractTypeSpec xs

-- createTStackFrames
createTStackFrames :: FunctionInfos -> [TStackFrame]
createTStackFrames fi = map createTStackFrame $ Map.elems fi

createTStackFrame :: FunctionInfo -> TStackFrame
createTStackFrame (FunctionInfo name resultType frame _) = 
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
                       undefNode) : result : frame))
                 [] undefNode)
              undefNode)]

        [(Just (CDeclr (Just (frameIdent name)) [] Nothing [] undefNode), Nothing, Nothing)]
        undefNode)
	where
		result = 	CDecl [CTypeSpec resultType] [(Just (CDeclr (Just (ident "ec_result")) [] Nothing [] undefNode), Nothing, Nothing)] undefNode
                       

ident s = Ident s 0 undefNode
frameIdent funcName = ident $ frameName funcName
frameName funcName = "frame_" ++ funcName ++ "_t"

