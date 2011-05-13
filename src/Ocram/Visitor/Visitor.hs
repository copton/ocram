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
	upCTranslUnit :: CTranslUnit -> downState -> upState -> upState
	upCTranslUnit _ _ = id

	upCExtDecl :: CExtDecl -> downState -> upState -> upState
	upCExtDecl _ _ = id

	upCFunDef :: CFunDef -> downState -> upState -> upState
	upCFunDef _ _ = id

	upCDecl :: CDecl -> downState -> upState -> upState
	upCDecl _ _ = id

	upCStructUnion :: CStructUnion -> downState -> upState -> upState
	upCStructUnion _ _ = id
	
	upCEnum :: CEnum -> downState -> upState -> upState
	upCEnum _ _ = id

	upCDeclr :: CDeclr -> downState -> upState -> upState
	upCDeclr _ _ = id

	upCStat :: CStat -> downState -> upState -> upState
	upCStat _ _ = id

	upCExpr :: CExpr -> downState -> upState -> upState
	upCExpr _ _ = id

	upIdent :: Ident -> downState -> upState -> upState
	upIdent _ _ = id

	upCDerivedDeclr :: CDerivedDeclr -> downState -> upState -> upState
	upCDerivedDeclr _ _ = id

	upCInit :: CInit -> downState -> upState -> upState
	upCInit _ _ = id

-- map {{{2
	mapCTranslUnit :: CTranslUnit -> downState -> upState -> (Maybe CTranslUnit, upState)
	mapCTranslUnit x d s = (Nothing, upCTranslUnit x d s)

	mapCExtDecl :: CExtDecl -> downState -> upState -> (Maybe CExtDecl, upState)
	mapCExtDecl x d s = (Nothing, upCExtDecl x d s)

	mapCFunDef :: CFunDef -> downState -> upState -> (Maybe CFunDef, upState)
	mapCFunDef x d s = (Nothing, upCFunDef x d s)

	mapCDecl :: CDecl -> downState -> upState -> (Maybe CDecl, upState)
	mapCDecl x d s = (Nothing, upCDecl x d s)

	mapCStructUnion :: CStructUnion -> downState -> upState -> (Maybe CStructUnion, upState)
	mapCStructUnion x d s = (Nothing, upCStructUnion x d s)
	
	mapCEnum :: CEnum -> downState -> upState -> (Maybe CEnum, upState)
	mapCEnum x d s = (Nothing, upCEnum x d s)

	mapCDeclr :: CDeclr -> downState -> upState -> (Maybe CDeclr, upState)
	mapCDeclr x d s = (Nothing, upCDeclr x d s)

	mapCStat :: CStat -> downState -> upState -> (Maybe CStat, upState)
	mapCStat x d s = (Nothing, upCStat x d s)

	mapCExpr :: CExpr -> downState -> upState -> (Maybe CExpr, upState)
	mapCExpr x d s = (Nothing, upCExpr x d s)

	mapIdent :: Ident -> downState -> upState -> (Maybe Ident, upState)
	mapIdent x d s = (Nothing, upIdent x d s)

	mapCDerivedDeclr :: CDerivedDeclr -> downState -> upState -> (Maybe CDerivedDeclr, upState)
	mapCDerivedDeclr x d s = (Nothing, upCDerivedDeclr x d s)

	mapCInit :: CInit -> downState -> upState -> (Maybe CInit, upState)
	mapCInit x d s = (Nothing, upCInit x d s)

-- cross {{{2
	-- external declarations of a translation unit
	crossCExtDecl :: CExtDecl -> downState -> upState -> (Maybe [CExtDecl], downState, upState)
	crossCExtDecl _ d u = (Nothing, d, u)

	-- 
	crossCDeclMember :: (Maybe CDeclr, Maybe CInit, Maybe CExpr) -> downState -> upState -> (Maybe [(Maybe CDeclr, Maybe CInit, Maybe CExpr)], downState, upState)
	crossCDeclMember _ d u = (Nothing, d, u)

  -- parameters of an old style function declaration
	crossIdent :: Ident -> downState -> upState -> (Maybe [Ident], downState, upState)
	crossIdent _ d u = (Nothing, d, u)

  --
	crossCDecl :: CDecl -> downState -> upState -> (Maybe [CDecl], downState, upState)
	crossCDecl _ d u = (Nothing, d, u)

  --
	crossCDerivedDeclr :: CDerivedDeclr -> downState -> upState -> (Maybe [CDerivedDeclr], downState, upState)
	crossCDerivedDeclr _ d u = (Nothing, d, u)

	-- expressions in a sequence of comma operators
	-- parameters of a function call
	crossCExpr :: CExpr -> downState -> upState -> (Maybe [CExpr], downState, upState)
	crossCExpr _ d u = (Nothing, d, u)

	-- items of a compound statement
	crossCBlockItem :: CBlockItem -> downState -> upState -> (Maybe [CBlockItem], downState, upState)
	crossCBlockItem _ d u = (Nothing, d, u)

  --
	crossCInitListMember :: ([CDesignator], CInit) -> downState -> upState -> (Maybe [([CDesignator], CInit)], downState, upState)
	crossCInitListMember _ d u = (Nothing, d, u)
