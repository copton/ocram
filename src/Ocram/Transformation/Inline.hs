module Ocram.Transformation.Inline 
-- exports {{{1
(
	transform
) where

-- imports {{{1
import Ocram.Transformation.Inline.Names
import Ocram.Transformation.Inline.Types

import Ocram.Transformation.Util (un, ident)

import Ocram.Types
import Ocram.Visitor
import Ocram.Symbols (symbol)
import Ocram.Util ((?:))
import Ocram.Query (getCallChain)

import Language.C.Syntax.AST
import Language.C.Data.Ident

import Data.Monoid
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as List

import Control.Exception (assert)

-- transform {{{1
transform :: Context -> Result OutputAst
transform ctx = do
	valid_ast <- getValidAst ctx
	cg <- getCallGraph ctx
	cf <- getCriticalFunctions ctx
	bf <- getBlockingFunctions ctx
	sr <- getStartRoutines ctx
	df <- getDefinedFunctions ctx
	let ast = getAst valid_ast
	return $ OutputAst $ transform' bf cf ast

transform' bf cf ast =
	fromJust $ fst $ result
	where
		result :: (Maybe CTranslUnit, UpState)
		result = traverseCTranslUnit ast $ DownState bf cf Set.empty

-- Types {{{2
type SymTab = Set.Set Symbol

data DownState = DownState {
		  dBf :: BlockingFunctions
		, dCf :: CriticalFunctions
		, dSt :: SymTab
	}

data UpState = UpState {
		dummy :: ()
	}

-- Visitor {{{2
instance Monoid UpState where
	mempty = UpState ()
	mappend _ _ = mempty

instance DownVisitor DownState where
	downCFunDef fd d 
		| Set.member (symbol fd) (dCf d) = d {dSt = params}
		| otherwise = d
		where
			params = Set.fromList $ map symbol $ extractParams' fd

instance UpVisitor DownState UpState where
	-- rewrite declarations of blocking functions {{{3
	mapCExtDecl (CDeclExt cd) d _
		| Set.member (symbol cd) (dBf d) = (Just $ CDeclExt cd', mempty)
		| otherwise = (Nothing, mempty)
		where
			cd' = createBlockingFunctionDeclr cd

	mapCExtDecl _ _ _ = (Nothing, mempty)

	-- remove critical functions {{{3
	crossCExtDecl (CFDefExt fd) d u
		| Set.member (symbol fd) (dCf d) = (Just [], d, u)
		| otherwise = (Nothing, d, u)
	
	crossCExtDecl _ d u = (Nothing, d, u)

-- support {{{2
-- util {{{3
-- extractParams {{{4
extractParams' :: CFunDef -> [CDecl]
extractParams' (CFunDef _ (CDeclr _ [cfd] _ _ _) [] _ _) = extractParams cfd

extractParams :: CDerivedDeclr -> [CDecl]
extractParams (CFunDeclr (Right (ps, False)) _ _) = ps

-- blocking functions {{{3
createBlockingFunctionDeclr :: CDecl -> CDecl
createBlockingFunctionDeclr cd = let fName = symbol cd in
   CDecl [CTypeSpec (CVoidType un)] [(Just (CDeclr (Just (ident fName)) [CFunDeclr (Right ([CDecl [CTypeSpec (CTypeDef (ident (frameType fName)) un)] [(Just (CDeclr (Just (ident frameParam)) [CPtrDeclr [] un] Nothing [] un), Nothing, Nothing)] un], False)) [] un] Nothing [] un), Nothing, Nothing)] un

