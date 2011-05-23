-- create function infos
module Ocram.Transformation.Inline.Step2
-- exports {{{1
(
	step2
) where

-- imports {{{1
import Ocram.Transformation.Inline.Types

import Ocram.Types
import Ocram.Visitor
import Ocram.Symbols (symbol)

import Language.C.Syntax.AST

import Data.Monoid
import qualified Data.Map as Map

-- step2 {{{1
step2 :: [CDecl] -> [CFunDef] -> FunctionInfos
step2 bfs cfs = mconcat (bffis ++ cffis)
	where
		bffis = map decl2fi bfs
		cffis = map cffi cfs
		cffi cf = uFi $ snd $ traverseCFunDef cf $ DownState Map.empty []

data DownState = DownState {
		dSt :: SymTab
	, dPs :: [CDecl]	
	}

data UpState = UpState {
	  uFi	:: FunctionInfos
	, uSt :: SymTab
	}

instance Monoid UpState where
	mempty = UpState mempty mempty
	mappend (UpState a b) (UpState a' b') = UpState (mappend a a') (mappend b b')

instance DownVisitor DownState where
	-- add parameters to symbol table
	downCFunDef fd _ = DownState (params2SymTab params) params
		where
			params = extractParams fd

	-- add declarations to symbol table {{{3
	downCBlockItem (CBlockDecl cd) d = d {dSt = Map.insert (symbol cd) cd (dSt d)}

	downCBlockItem _ d = d

instance UpVisitor DownState UpState where
	-- pass declarations from down state to up state
	upCBlockItem o@(CBlockDecl _) d u = (o, u `mappend` UpState mempty (dSt d))

	upCBlockItem o _ u = (o, u)

	-- create function info entry {{{3
	upCFunDef o@(CFunDef tss _ _ body _) d u = (o, UpState (Map.singleton name fi) mempty)
		where
			name = symbol o
			fi = FunctionInfo (extractTypeSpec tss) (dPs d) (dSt d `mappend` uSt u) (Just body)

instance ListVisitor DownState UpState where
	-- remove variable declarations {{{3
	nextCBlockItem (CBlockDecl _) d u = ([], d, u)

	nextCBlockItem o d u = ([o], d, u)

-- utils {{{1
extractParams :: CFunDef -> [CDecl]
extractParams (CFunDef _ (CDeclr _ [cfd] _ _ _) [] _ _) = extractParams' cfd

extractParams' :: CDerivedDeclr -> [CDecl]
extractParams' (CFunDeclr (Right (ps, False)) _ _) = ps

params2SymTab :: [CDecl] -> SymTab
params2SymTab params = foldl add Map.empty params
	where
		add m cd = Map.insert (symbol cd) cd m

decl2fi :: CDecl -> FunctionInfos
decl2fi cd@(CDecl tss [(Just (CDeclr _ [cfd] _ _ _), Nothing, Nothing)] _) = Map.singleton (symbol cd) fi
	
	where
		fi = FunctionInfo (extractTypeSpec tss) params (params2SymTab params) Nothing	
		params = extractParams' cfd

extractTypeSpec :: [CDeclSpec] -> CTypeSpec
extractTypeSpec [] = error "assertion failed: type specifier expected"
extractTypeSpec ((CTypeSpec ts):xs) = ts
extractTypeSpec (_:xs) = extractTypeSpec xs

