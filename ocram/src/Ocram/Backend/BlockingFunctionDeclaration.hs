module Ocram.Backend.BlockingFunctionDeclaration
-- exports {{{1
(
  blocking_function_declaration
) where

-- imports {{{1
import Language.C.Syntax.AST
import Language.C.Data.Node (undefNode)
import Language.C.Data.Ident (internalIdent)
import Ocram.Symbols (symbol)
import Ocram.Names (tframe)

blocking_function_declaration :: CDecl -> CDecl
blocking_function_declaration cd = decl
  where
    un = undefNode
    decl = CDecl ts [(Just declr, Nothing, Nothing)] un
    ts = [CTypeSpec (CVoidType un)]
    declr = CDeclr iden fdeclr Nothing [] un
    iden = Just (internalIdent fName) 
    fName = symbol cd 
    fdeclr = [CFunDeclr (Right ([param], False)) [] un]
    param = CDecl ts' [(Just declr', Nothing, Nothing)] un 
    ts' = [CTypeSpec (CTypeDef (internalIdent (tframe fName)) un)]
    declr' = CDeclr Nothing [CPtrDeclr [] un] Nothing [] un
