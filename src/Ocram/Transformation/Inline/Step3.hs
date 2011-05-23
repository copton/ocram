-- add tstack structures and variables
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
import Ocram.Util ((?:))
import Ocram.Query (getCallChain)

import Language.C.Syntax.AST
import Language.C.Data.Ident

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as List

-- step3 {{{1
step3 :: CriticalFunctions -> StartRoutines -> CallGraph -> FunctionInfos -> Ast -> Ast
step3 cf sr cg fis (CTranslUnit decls ni) = CTranslUnit (frames ++ stacks ++ decls) ni
	where
		frames = createTStackFrames cf sr cg fis
		stacks = map createTStack $ Set.elems sr

-- createTStack :: Symbol -> CExtDecl {{{2
createTStack :: Symbol -> CExtDecl
createTStack fName = CDeclExt (CDecl [CTypeSpec (CTypeDef (ident (frameType fName)) un)] [(Just (CDeclr (Just (ident (stackVar fName))) [] Nothing [] un), Nothing, Nothing)] un)

-- createTStackFrames :: StartRoutines -> CallGraph -> FunctionInfos -> [CExtDecl] {{{2
createTStackFrames :: CriticalFunctions -> StartRoutines -> CallGraph -> FunctionInfos -> [CExtDecl]
createTStackFrames cf sr cg fis = frames
	where
		frames = map (createTStackFrame cf sr cg) $ List.reverse fipairs
		fipairs = fst $ foldl fld ([], Set.empty) $ concatMap (List.reverse . getCallChain cg) $ Set.elems sr
		fld (lst, set) fname
			| Set.member fname set = (lst, set)
			| otherwise = case Map.lookup fname fis of
					Nothing -> (lst, set)
					(Just fi) -> ((fname, fi) : lst, Set.insert fname set)
		
-- createTStackFrame {{{3
createTStackFrame :: CriticalFunctions -> StartRoutines -> CallGraph -> (Symbol, FunctionInfo) -> CExtDecl
createTStackFrame cf sr cg (name, fi@(FunctionInfo resultType _ vars _)) = 
	 CDeclExt
		 (CDecl
				[CStorageSpec (CTypedef un),
				 CTypeSpec
					 (CSUType
							(CStruct CStructTag Nothing
								 (Just (
										 continuation ?: result ?: nestedFrames ?: Map.elems vars))
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

		nestedFrames = createNestedFramesUnion cf cg (name, fi)

-- createNestedFramesUnion {{{3
createNestedFramesUnion :: CriticalFunctions -> CallGraph -> (Symbol, FunctionInfo) -> Maybe CDecl
createNestedFramesUnion cf cg (name, fi) = result
	where
		result = if null entries then Nothing else Just createDecl
		entries = map createEntry $ filter (flip Set.member cf) $ Set.elems $ cgCallees $ mlookup "XXX 2" name cg
		createEntry sym = CDecl [CTypeSpec (CTypeDef (ident (frameType sym)) un)]
											[(Just (CDeclr (Just (ident sym)) [] Nothing [] un), Nothing, Nothing)] un 
		createDecl = CDecl [CTypeSpec (CSUType (CStruct CUnionTag Nothing (Just entries) [] un) un)] [(Just (CDeclr (Just (ident frameUnion)) [] Nothing [] un), Nothing, Nothing)] un

mlookup s k m = case Map.lookup k m of
	Nothing -> error s
	Just x -> x
