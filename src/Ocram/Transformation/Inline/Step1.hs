-- prepare AST
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

import Language.C.Syntax.AST
import Language.C.Data.Ident

import Data.Monoid
import qualified Data.Set as Set
import qualified Data.Map as Map

-- step1 {{{1
step1 :: BlockingFunctions -> CriticalFunctions -> Ast -> (Ast, [CDecl], [CFunDef])
step1 bf cf ast = (ast', bf', cf')
	where
		(ast', (UpState bf' cf')) = traverseCTranslUnit ast (DownState bf cf)

-- Types {{{2

data DownState = DownState {
		  dBf :: BlockingFunctions
		, dCf :: CriticalFunctions
	}

data UpState = UpState {
		uBf :: [CDecl]
	, uCf :: [CFunDef]
	}

-- Visitor {{{2
instance Monoid UpState where
	mempty = UpState mempty mempty
	mappend (UpState a b) (UpState a' b') = UpState (mappend a a') (mappend b b')

instance DownVisitor DownState 

instance UpVisitor DownState UpState where
	-- rewrite function declarations of blocking functions
	upCExtDecl o@(CDeclExt cd) d _
		| Set.member (symbol cd) (dBf d) = (cd', UpState [cd] [])
		| otherwise = (o, mempty)
		where
			cd' = CDeclExt $ createBlockingFunctionDeclr cd

	upCExtDecl o _ u = (o, u)

instance ListVisitor DownState UpState where
	-- remove critical functions
	nextCExtDecl o@(CFDefExt fd) d u
		| Set.member (symbol fd) (dCf d) = ([], d, u {uCf = fd : (uCf u)})
		| otherwise = ([o], d, u)
	
	nextCExtDecl o d u = ([o], d, u)

-- blocking function declr {{{2
createBlockingFunctionDeclr :: CDecl -> CDecl
createBlockingFunctionDeclr cd = let fName = symbol cd in
   CDecl [CTypeSpec (CVoidType un)] [(Just (CDeclr (Just (ident fName)) [CFunDeclr (Right ([CDecl [CTypeSpec (CTypeDef (ident (frameType fName)) un)] [(Just (CDeclr (Just (ident frameParam)) [CPtrDeclr [] un] Nothing [] un), Nothing, Nothing)] un], False)) [] un] Nothing [] un), Nothing, Nothing)] un


