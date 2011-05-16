module Ocram.Transformation.Inline.Step2
-- exports {{{1
(
	step2
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

-- step2 {{{1
step2 :: StartRoutines -> CallGraph -> FunctionInfos -> Ast -> Ast
step2 sr cg fis (CTranslUnit decls ni) = CTranslUnit (frames ++ stacks ++ decls) ni
	where
		frames = createTStackFrames sr cg fis
		stacks = map createTStack $ Set.elems sr

-- createTStack :: Symbol -> CExtDecl {{{2
createTStack :: Symbol -> CExtDecl
createTStack fName = CDeclExt (CDecl [CTypeSpec (CTypeDef (ident (frameType fName)) un)] [(Just (CDeclr (Just (ident (stackVar fName))) [] Nothing [] un), Nothing, Nothing)] un)

-- createTStackFrames :: StartRoutines -> CallGraph -> FunctionInfos -> [CExtDecl] {{{2
createTStackFrames :: StartRoutines -> CallGraph -> FunctionInfos -> [CExtDecl]
createTStackFrames sr cg fis = frames
	where
		frames = map (createTStackFrame sr cg) fipairs
		fipairs = fst $ foldl fld ([], Set.empty) $ concatMap (getCallChain cg) $ Set.elems sr
		fld (lst, set) fname
			| Set.member fname set = (lst, set)
			| otherwise = case Map.lookup fname fis of
					Nothing -> (lst, set)
					(Just fi) -> ((fname, fi) : lst, Set.insert fname set)
		
-- createTStackFrame {{{3
createTStackFrame :: StartRoutines -> CallGraph -> (Symbol, FunctionInfo) -> CExtDecl
createTStackFrame sr cg (name, fi@(FunctionInfo resultType vars _)) = 
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

		nestedFrames = createNestedFramesUnion cg (name, fi)

-- createNestedFramesUnion {{{3
createNestedFramesUnion :: CallGraph -> (Symbol, FunctionInfo) -> Maybe CDecl
createNestedFramesUnion cg (name, fi) = result
	where
		result = if null entries then Nothing else Just createDecl
		entries = map createEntry $ Set.elems $ cgCallees $ cg Map.! name
		createEntry sym = CDecl [CTypeSpec (CTypeDef (ident (frameType sym)) un)]
											[(Just (CDeclr (Just (ident sym)) [] Nothing [] un), Nothing, Nothing)] un 
		createDecl = CDecl [CTypeSpec (CSUType (CStruct CUnionTag Nothing (Just entries) [] un) un)] [(Just (CDeclr (Just (ident frameUnion)) [] Nothing [] un), Nothing, Nothing)] un
