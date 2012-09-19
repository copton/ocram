module Ocram.Backend.BlockingFunctionDeclaration
-- exports {{{1
(
  blocking_function_declarations
) where

-- imports {{{1
import Language.C.Data.Ident (internalIdent)
import Language.C.Data.Node (undefNode)
import Language.C.Syntax.AST
import Ocram.Names (tframe)
import Ocram.Symbols (symbol)
import Ocram.Symbols (Symbol)

import qualified Data.Map as M

blocking_function_declarations :: M.Map Symbol CDecl -> [CDecl]
blocking_function_declarations = map blockingFunctionDeclaration . M.elems

blockingFunctionDeclaration :: CDecl -> CDecl -- {{{1
blockingFunctionDeclaration cd = decl
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
