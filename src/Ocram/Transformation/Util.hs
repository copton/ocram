module Ocram.Transformation.Util (
	ident, un, tStackAccess
) where

import Language.C.Data.Ident (Ident(Ident))
import Language.C.Data.Node (undefNode)
import Language.C.Syntax.AST
import Ocram.Names (frameType, contVar, contFrameVar)

ident s = Ident s 0 un
un = undefNode

tStackAccess :: String -> String -> CExpr
tStackAccess fName vName = CMember (CCast (CDecl [CTypeSpec (CTypeDef (ident (frameType fName)) un)] [(Just (CDeclr Nothing [CPtrDeclr [] un] Nothing [] un), Nothing, Nothing)] un) (CMember (CVar (ident contVar) un) (ident contFrameVar) False un) un) (ident vName) True un
