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

	crossCExtDecl :: CExtDecl -> downState -> (Maybe [CExtDecl], downState)
	crossCExtDecl _ d = (Nothing, d)

	crossCDeclMember :: (Maybe CDeclr, Maybe CInit, Maybe CExpr) -> downState -> (Maybe [(Maybe CDeclr, Maybe CInit, Maybe CExpr)], downState)
	crossCDeclMember _ d = (Nothing, d)

	crossIdent :: Ident -> downState -> (Maybe [Ident], downState)
	crossIdent _ d = (Nothing, d)

	crossCDecl :: CDecl -> downState -> (Maybe [CDecl], downState)
	crossCDecl _ d = (Nothing, d)

	crossCDerivedDeclr :: CDerivedDeclr -> downState -> (Maybe [CDerivedDeclr], downState)
	crossCDerivedDeclr _ d = (Nothing, d)

	crossCExpr :: CExpr -> downState -> (Maybe [CExpr], downState)
	crossCExpr _ d = (Nothing, d)

	crossCBlockItem :: CBlockItem -> downState -> (Maybe [CBlockItem], downState)
	crossCBlockItem _ d = (Nothing, d)

	crossCInitListMember :: ([CDesignator], CInit) -> downState -> (Maybe [([CDesignator], CInit)], downState)
	crossCInitListMember _ d = (Nothing, d)

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

	mapCTranslUnit :: CTranslUnit -> downState -> [upState] -> (Maybe CTranslUnit, upState)
	mapCTranslUnit x d s = (Nothing, upCTranslUnit x d s)

	mapCExtDecl :: CExtDecl -> downState -> [upState] -> (Maybe CExtDecl, upState)
	mapCExtDecl x d s = (Nothing, upCExtDecl x d s)

	mapCFunDef :: CFunDef -> downState -> [upState] -> (Maybe CFunDef, upState)
	mapCFunDef x d s = (Nothing, upCFunDef x d s)

	mapCDecl :: CDecl -> downState -> [upState] -> (Maybe CDecl, upState)
	mapCDecl x d s = (Nothing, upCDecl x d s)

	mapCStructUnion :: CStructUnion -> downState -> [upState] -> (Maybe CStructUnion, upState)
	mapCStructUnion x d s = (Nothing, upCStructUnion x d s)
	
	mapCEnum :: CEnum -> downState -> [upState] -> (Maybe CEnum, upState)
	mapCEnum x d s = (Nothing, upCEnum x d s)

	mapCDeclr :: CDeclr -> downState -> [upState] -> (Maybe CDeclr, upState)
	mapCDeclr x d s = (Nothing, upCDeclr x d s)

	mapCStat :: CStat -> downState -> [upState] -> (Maybe CStat, upState)
	mapCStat x d s = (Nothing, upCStat x d s)

	mapCExpr :: CExpr -> downState -> [upState] -> (Maybe CExpr, upState)
	mapCExpr x d s = (Nothing, upCExpr x d s)

	mapIdent :: Ident -> downState -> [upState] -> (Maybe Ident, upState)
	mapIdent x d s = (Nothing, upIdent x d s)

	mapCDerivedDeclr :: CDerivedDeclr -> downState -> [upState] -> (Maybe CDerivedDeclr, upState)
	mapCDerivedDeclr x d s = (Nothing, upCDerivedDeclr x d s)

	mapCInit :: CInit -> downState -> [upState] -> (Maybe CInit, upState)
	mapCInit x d s = (Nothing, upCInit x d s)
