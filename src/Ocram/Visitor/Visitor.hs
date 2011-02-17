{-# LANGUAGE MultiParamTypeClasses #-}

module Ocram.Visitor.Visitor (
	DownVisitor(..),
	UpVisitor(..)
) where

import Language.C.Syntax.AST
import Language.C.Data.Ident
import Data.Monoid

class DownVisitor downState where
	downCTranslUnit :: CTranslUnit -> downState -> downState
	downCTranslUnit _ = id

	downCExtDecl :: CExtDecl -> downState -> downState
	downCExtDecl _  = id

	downCFunDef :: CFunDef -> downState -> downState
	downCFunDef _ = id

	downCDecl :: CDecl -> downState -> downState
	downCDecl _ = id

	downCStructUnion :: CStructUnion -> downState -> downState
	downCStructUnion _ = id
	
	downCEnum :: CEnum -> downState -> downState
	downCEnum _ = id

	downCDeclr :: CDeclr -> downState -> downState
	downCDeclr _ = id

	downCStat :: CStat -> downState -> downState
	downCStat _ = id

	downCExpr :: CExpr -> downState -> downState
	downCExpr _ = id

	downIdent :: Ident -> downState -> downState
	downIdent _ = id

	downCDerivedDeclr :: CDerivedDeclr -> downState -> downState
	downCDerivedDeclr _ = id

	downCInit :: CInit -> downState -> downState
	downCInit _ = id

class (DownVisitor downState, Monoid upState) => UpVisitor downState upState where
	upCTranslUnit :: CTranslUnit -> downState -> [upState] -> upState
	upCTranslUnit _ _ = mconcat

	upCExtDecl :: CExtDecl -> downState -> [upState] -> upState
	upCExtDecl _ _ = mconcat

	upCFunDef :: CFunDef -> downState -> [upState] -> upState
	upCFunDef _ _ = mconcat

	upCDecl :: CDecl -> downState -> [upState] -> upState
	upCDecl _ _ = mconcat

	upCStructUnion :: CStructUnion -> downState -> [upState] -> upState
	upCStructUnion _ _ = mconcat
	
	upCEnum :: CEnum -> downState -> [upState] -> upState
	upCEnum _ _ = mconcat

	upCDeclr :: CDeclr -> downState -> [upState] -> upState
	upCDeclr _ _ = mconcat

	upCStat :: CStat -> downState -> [upState] -> upState
	upCStat _ _ = mconcat

	upCExpr :: CExpr -> downState -> [upState] -> upState
	upCExpr _ _ = mconcat

	upIdent :: Ident -> downState -> [upState] -> upState
	upIdent _ _ = mconcat

	upCDerivedDeclr :: CDerivedDeclr -> downState -> [upState] -> upState
	upCDerivedDeclr _ _ = mconcat

	upCInit :: CInit -> downState -> [upState] -> upState
	upCInit _ _ = mconcat
