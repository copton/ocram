-- traverse ast
module Ocram.Transformation.Inline.Step1
-- exports {{{1
(
	step1
) where

-- imports {{{1
import Ocram.Transformation.Inline.Names
import Ocram.Transformation.Inline.Types

import Ocram.Transformation.Util (un, ident)

import Ocram.Types
import Ocram.Visitor
import Ocram.Symbols (symbol)
import Ocram.Util (mapt2)

import Language.C.Syntax.AST
import Language.C.Data.Ident

import Data.Monoid
import Data.Maybe (fromJust, isJust)
import qualified Data.Set as Set
import qualified Data.Map as Map

import Control.Exception (assert)
import Debug.Trace (trace)

-- step1 {{{1
step1 :: BlockingFunctions -> CriticalFunctions -> Ast -> (Ast, FunctionInfos)
step1 bf cf ast = mapt2 (id, uFis) result
	where
		result = traverseCTranslUnit ast d
		d = DownState bf cf Map.empty [] Nothing

-- Types {{{2

data DownState = DownState {
		  dBf :: BlockingFunctions
		, dCf :: CriticalFunctions
		, dSt :: SymTab
		, dPs :: [CDecl]
		, dName :: Maybe (Symbol)
	}

data UpState = UpState {
		  uFis :: FunctionInfos
		, uSt :: SymTab
	}

-- Visitor {{{2
instance Monoid UpState where
	mempty = UpState mempty mempty
	mappend (UpState a b) (UpState a' b') = UpState (mappend a a') (mappend b b')

instance DownVisitor DownState where
	-- start handling of critical functions {{{3
	-- add parameters to symbol table
	downCFunDef fd d 
		| Set.member (symbol fd) (dCf d) = d {
					dSt = params2SymTab params
				,	dPs = params
				, dName = Just (symbol fd)
			}
		| otherwise = d
		where
			params = extractParams fd

	-- add declarations to symbol table {{{3
	downCBlockItem (CBlockDecl cd) d
		| isJust $ dName d = d {dSt = Map.insert (symbol cd) cd (dSt d)}
		| otherwise = d

	downCBlockItem _ d = d

instance UpVisitor DownState UpState where
	-- create function info entry for critical function {{{3
	upCFunDef o@(CFunDef tss _ _ body _) d u
		| isJust $ dName d =
			assert (fromJust (dName d) == name) $
			(o, UpState (Map.singleton name fi) mempty)
		| otherwise = (o, u)
		where
			name = symbol o
			fi = FunctionInfo (extractTypeSpec tss) (dPs d) (uSt u) (Just body)

	-- pass declarations from down state to up state
	upCBlockItem o@(CBlockDecl _) d u
		| isJust $ dName d = (o, u `mappend` UpState mempty (dSt d))
		| otherwise = (o, u)

	upCBlockItem o _ u = (o, u)

	-- rewrite declarations of blocking functions {{{3
	-- create function info entry for blocking functions
	upCExtDecl o@(CDeclExt cd) d u
		| Set.member (symbol cd) (dBf d) = (cd', u)
		| otherwise = (o, u)
		where
			cd' = CDeclExt $ createBlockingFunctionDeclr cd
			u = UpState (Map.singleton (symbol cd) (decl2fi cd)) mempty

	upCExtDecl o _ u = (o, u)

instance ListVisitor DownState UpState where
	-- remove critical functions {{{3
	nextCExtDecl o@(CFDefExt fd) d u
		| Set.member (symbol fd) (dCf d) = ([], d, u)
		| otherwise = ([o], d, u)
	
	nextCExtDecl o d u = ([o], d, u)

	-- remove declarations {{{3
	nextCBlockItem o@(CBlockDecl _) d u
		| isJust $ dName d = ([], d, u)
		| otherwise = ([o], d, u)

	nextCBlockItem o d u = ([o], d, u)

-- support {{{2
-- util {{{3
-- function info {{{4
extractParams :: CFunDef -> [CDecl]
extractParams (CFunDef _ (CDeclr _ [cfd] _ _ _) [] _ _) = extractParams' cfd

extractParams' :: CDerivedDeclr -> [CDecl]
extractParams' (CFunDeclr (Right (ps, False)) _ _) = ps

params2SymTab :: [CDecl] -> SymTab
params2SymTab params = foldl add Map.empty params
	where
		add m cd = Map.insert (symbol cd) cd m

decl2fi :: CDecl -> FunctionInfo
decl2fi (CDecl tss [(Just (CDeclr _ [cfd] _ _ _), Nothing, Nothing)] _) =
	FunctionInfo (extractTypeSpec tss) params (params2SymTab params) Nothing
	where
		params = extractParams' cfd

extractTypeSpec :: [CDeclSpec] -> CTypeSpec
extractTypeSpec [] = error "assertion failed: type specifier expected"
extractTypeSpec ((CTypeSpec ts):xs) = ts
extractTypeSpec (_:xs) = extractTypeSpec xs

-- blocking function declr {{{3
createBlockingFunctionDeclr :: CDecl -> CDecl
createBlockingFunctionDeclr cd = let fName = symbol cd in
   CDecl [CTypeSpec (CVoidType un)] [(Just (CDeclr (Just (ident fName)) [CFunDeclr (Right ([CDecl [CTypeSpec (CTypeDef (ident (frameType fName)) un)] [(Just (CDeclr (Just (ident frameParam)) [CPtrDeclr [] un] Nothing [] un), Nothing, Nothing)] un], False)) [] un] Nothing [] un), Nothing, Nothing)] un


