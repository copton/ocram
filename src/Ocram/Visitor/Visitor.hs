module Ocram.Visitor.Visitor (
	  DownVisitor(..), UpVisitor(..), ListVisitor(..)
	, UpHandler, DownHandler, NextHandler
) where

import Language.C.Syntax.AST
import Language.C.Data.Ident
import Data.Monoid

-- types {{{1
type DownHandler o d = o -> d -> d
type UpHandler o d u = o -> d -> u -> (o, u)
type NextHandler o d u = o -> d -> u -> ([o], d, u)

-- DownVisitor {{{1
class DownVisitor downState where
	downCTranslUnit :: DownHandler CTranslUnit downState
	downCTranslUnit _ = id

	downCExtDecl :: DownHandler CExtDecl downState
	downCExtDecl _  = id

	downCFunDef :: DownHandler CFunDef downState
	downCFunDef _ = id

	downCDecl :: CDecl -> downState -> downState
	downCDecl _ = id

	downCStructUnion :: DownHandler CStructUnion downState
	downCStructUnion _ = id
	
	downCEnum :: DownHandler CEnum downState
	downCEnum _ = id

	downCDeclr :: DownHandler CDeclr downState
	downCDeclr _ = id

	downCStat :: DownHandler CStat downState
	downCStat _ = id

	downCBlockItem :: DownHandler CBlockItem downState
	downCBlockItem _ = id

	downCExpr :: DownHandler CExpr downState
	downCExpr _ = id

	downCDerivedDeclr :: DownHandler CDerivedDeclr downState
	downCDerivedDeclr _ = id

	downCInit :: DownHandler CInit downState
	downCInit _ = id

-- UpVisitor {{{1
class (Monoid upState) => UpVisitor downState upState where
	upCTranslUnit :: UpHandler CTranslUnit downState upState
	upCTranslUnit o _ u = (o, u)

	upCExtDecl :: UpHandler CExtDecl downState upState
	upCExtDecl o _ u = (o, u)

	upCFunDef :: UpHandler CFunDef downState upState
	upCFunDef o _ u = (o, u)

	upCDecl :: UpHandler CDecl downState upState
	upCDecl o _ u = (o, u)

	upCStructUnion :: UpHandler CStructUnion downState upState
	upCStructUnion o _ u = (o, u)
	
	upCEnum :: UpHandler CEnum downState upState
	upCEnum o _ u = (o, u)

	upCDeclr :: UpHandler CDeclr downState upState
	upCDeclr o _ u = (o, u)

	upCStat :: UpHandler CStat downState upState
	upCStat o _ u = (o, u)

	upCBlockItem :: UpHandler CBlockItem downState upState
	upCBlockItem o _ u = (o, u)

	upCExpr :: UpHandler CExpr downState upState
	upCExpr o _ u = (o, u)

	upCDerivedDeclr :: UpHandler CDerivedDeclr downState upState
	upCDerivedDeclr o _ u = (o, u)

	upCInit :: UpHandler CInit downState upState
	upCInit o _ u = (o, u)

-- ListVisitor {{{1
class (Monoid upState) => ListVisitor downState upState where
	nextCExtDecl :: NextHandler CExtDecl downState upState
	nextCExtDecl o d u = ([o], d, u)

	nextCDecl :: NextHandler CDecl downState upState		
	nextCDecl o d u = ([o], d, u)

	nextCExpr :: NextHandler CExpr downState upState
	nextCExpr o d u = ([o], d, u)
	
	nextCBlockItem :: NextHandler CBlockItem downState upState
	nextCBlockItem o d u = ([o], d, u)

	nextCDerivedDeclr :: NextHandler CDerivedDeclr downState upState
	nextCDerivedDeclr o d u = ([o], d, u)

	nextCInitListMember :: NextHandler ([CDesignator], CInit) downState upState
	nextCInitListMember o d u = ([o], d, u)

	nextCDeclMember :: NextHandler (Maybe CDeclr, Maybe CInit, Maybe CExpr) downState upState
	nextCDeclMember o d u = ([o], d, u)
