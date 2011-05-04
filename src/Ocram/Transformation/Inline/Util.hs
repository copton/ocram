module Ocram.Transformation.Inline.Util
-- exports {{{1
(
	tStackAccess
) where

-- imports {{{1
import Language.C.Syntax.AST
import Ocram.Transformation.Util (un, ident)
import Ocram.Transformation.Inline.Names (frameType, contVar)

-- tStackAccess :: String -> String -> CExpr {{{1
-- tStackAccess :: String -> String -> CExpr
-- tStackAccess fName vName = CMember (CCast (CDecl [CTypeSpec (CTypeDef (ident (frameType fName)) un)] [(Just (CDeclr Nothing [CPtrDeclr [] un] Nothing [] un), Nothing, Nothing)] un) (CMember (CVar (ident contVar) un) (ident contFrameVar) False un) un) (ident vName) True un
tStackAccess = undefined
