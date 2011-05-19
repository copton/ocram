module Ocram.Visitor.Visitor (
	DownVisitor(..),
	UpVisitor(..)
) where

import Language.C.Syntax.AST
import Language.C.Data.Ident
import Data.Monoid

-- DownVisitor {{{1
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

	downCBlockItem :: CBlockItem -> downState -> downState
	downCBlockItem _ = id

	downCExpr :: CExpr -> downState -> downState
	downCExpr _ = id

	downIdent :: Ident -> downState -> downState
	downIdent _ = id

	downCDerivedDeclr :: CDerivedDeclr -> downState -> downState
	downCDerivedDeclr _ = id

	downCInit :: CInit -> downState -> downState
	downCInit _ = id

-- UpVisitor {{{1
class (DownVisitor downState, Monoid upState) => UpVisitor downState upState where
-- up {{{2
	upCTranslUnit :: CTranslUnit -> downState -> upState -> (CTranslUnit, upState)
	upCTranslUnit o _ u = (o, u)

	upCExtDecl :: CExtDecl -> downState -> upState -> (CExtDecl, upState)
	upCExtDecl o _ u = (o, u)

	upCFunDef :: CFunDef -> downState -> upState -> (CFunDef, upState)
	upCFunDef o _ u = (o, u)

	upCDecl :: CDecl -> downState -> upState -> (CDecl, upState)
	upCDecl o _ u = (o, u)

	upCStructUnion :: CStructUnion -> downState -> upState -> (CStructUnion, upState)
	upCStructUnion o _ u = (o, u)
	
	upCEnum :: CEnum -> downState -> upState -> (CEnum, upState)
	upCEnum o _ u = (o, u)

	upCDeclr :: CDeclr -> downState -> upState -> (CDeclr, upState)
	upCDeclr o _ u = (o, u)

	upCStat :: CStat -> downState -> upState -> (CStat, upState)
	upCStat o _ u = (o, u)

	upCBlockItem :: CBlockItem -> downState -> upState -> (CBlockItem, upState)
	upCBlockItem o _ u = (o, u)

	upCExpr :: CExpr -> downState -> upState -> (CExpr, upState)
	upCExpr o _ u = (o, u)

	upIdent :: Ident -> downState -> upState -> (Ident, upState)
	upIdent o _ u = (o, u)

	upCDerivedDeclr :: CDerivedDeclr -> downState -> upState -> (CDerivedDeclr, upState)
	upCDerivedDeclr o _ u = (o, u)

	upCInit :: CInit -> downState -> upState -> (CInit, upState)
	upCInit o _ u = (o, u)

