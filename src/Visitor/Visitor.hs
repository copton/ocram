module Visitor.Visitor (
    Visitor(..)
) where

import Language.C.Syntax.AST
import Language.C.Data.Ident

class Visitor state where
    handleCExtDecl :: CExtDecl -> state -> state
    handleCExtDecl _  = id

    handleCFunDef :: CFunDef -> state -> state
    handleCFunDef _ = id

    handleCDecl :: CDecl -> state -> state
    handleCDecl _ = id

    handleCStructUnion :: CStructUnion -> state -> state
    handleCStructUnion _ = id
    
    handleCEnum :: CEnum -> state -> state
    handleCEnum _ = id

    handleCDeclr :: CDeclr -> state -> state
    handleCDeclr _ = id

    handleCStat :: CStat -> state -> state
    handleCStat _ = id

    handleCExpr :: CExpr -> state -> state
    handleCExpr _ = id

    handleIdent :: Ident -> state -> state
    handleIdent _ = id

    handleCDerivedDeclr :: CDerivedDeclr -> state -> state
    handleCDerivedDeclr _ = id

    handleCInit :: CInit -> state -> state
    handleCInit _ = id
