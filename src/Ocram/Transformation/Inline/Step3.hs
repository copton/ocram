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
getCallChain = undefined

import Language.C.Syntax.AST
import Language.C.Data.Ident

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as List

import Control.Monad.Reader (asks)

-- step3 :: FunctionInfos -> Ast -> WR Ast {{{1
step3 :: FunctionInfos -> Ast -> WR Ast
step3 fis (CTranslUnit decls ni) = do
	frames <- createTStackFrames fis
	sr <- asks getStartRoutines
	let stacks = map createTStack $ Set.elems sr
	return $ CTranslUnit (frames ++ stacks ++ decls) ni

-- createTStack :: Symbol -> CExtDecl {{{2
createTStack :: Symbol -> CExtDecl
createTStack fName = CDeclExt (CDecl [CTypeSpec (CTypeDef (ident (frameType fName)) un)] [(Just (CDeclr (Just (ident (stackVar fName))) [] Nothing [] un), Nothing, Nothing)] un)

-- createTStackFrames :: FunctionInfos -> WR [CExtDecl] {{{3
createTStackFrames :: FunctionInfos -> WR [CExtDecl]
createTStackFrames fis = do
		fipairs <- createFiPairs fis
		frames <- mapM createTStackFrame $ List.reverse fipairs
		return frames

createFiPairs :: FunctionInfos -> WR [(Symbol, FunctionInfo)]
createFiPairs fis = do
	sr <- asks getStartRoutines
	cg <- asks getCallGraph
	return $ fst $ foldl fld ([], Set.empty) $ concatMap (List.reverse . getCallChain cg) $ Set.elems sr
	where
		fld (lst, set) fname
			| Set.member fname set = (lst, set)
			| otherwise = case Map.lookup fname fis of
					Nothing -> (lst, set)
					(Just fi) -> ((fname, fi) : lst, Set.insert fname set)
		
-- createTStackFrame :: (Symbol, FunctionInfo) -> WR CExtDecl {{{3
createTStackFrame :: (Symbol, FunctionInfo) -> WR CExtDecl
createTStackFrame (name, fi@(FunctionInfo resultType _ vars _)) = do
	nestedFrames <- createNestedFramesUnion (name, fi)
	sr <- asks getStartRoutines
	let result = case resultType of
		(CVoidType _) -> Nothing
		_ -> Just $ CDecl [CTypeSpec resultType] [(Just (CDeclr (Just (ident resVar)) [] Nothing [] un), Nothing, Nothing)] un
	let continuation = if Set.member name sr
		then Nothing
		else Just (CDecl [CTypeSpec (CVoidType un)] [(Just (CDeclr (Just (ident contVar)) [CPtrDeclr [] un] Nothing [] un), Nothing, Nothing)] un)
	return $
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

-- createNestedFramesUnion :: (Symbol, FunctionInfo) -> WR (Maybe CDecl) {{{3
createNestedFramesUnion :: (Symbol, FunctionInfo) -> WR (Maybe CDecl)
createNestedFramesUnion (name, fi) = do
	cf <- asks getCriticalFunctions
	cg <- asks getCallGraph
	let createEntry sym = CDecl [CTypeSpec (CTypeDef (ident (frameType sym)) un)]
											[(Just (CDeclr (Just (ident sym)) [] Nothing [] un), Nothing, Nothing)] un 
	let entries = map createEntry $ filter (flip Set.member cf) $ Set.elems $ cgCallees $ cg Map.! name
	let createDecl = CDecl [CTypeSpec (CSUType (CStruct CUnionTag Nothing (Just entries) [] un) un)] [(Just (CDeclr (Just (ident frameUnion)) [] Nothing [] un), Nothing, Nothing)] un
	return $ if null entries then Nothing else Just createDecl
