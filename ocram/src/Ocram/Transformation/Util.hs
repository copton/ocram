module Ocram.Transformation.Util 
-- exports {{{1
(
  ident, un,
  map_critical_functions
) where

-- imports {{{1
import Language.C.Data.Ident (Ident(Ident))
import Language.C.Data.Node (undefNode, NodeInfo)
import Language.C.Syntax.AST (CTranslationUnit(CTranslUnit), CFunDef, CExternalDeclaration(CFDefExt))
import Ocram.Analysis (CallGraph, is_critical)
import Ocram.Symbols (symbol)
import Ocram.Types (Ast)

ident :: String -> Ident -- {{{1
ident s = Ident s 0 un

un :: NodeInfo -- {{{1
un = undefNode

map_critical_functions :: CallGraph -> Ast -> (CFunDef -> CFunDef) -> Ast -- {{{1
map_critical_functions cg (CTranslUnit ds ni) f = CTranslUnit (map process ds) ni
  where
    process x@(CFDefExt fd)
      | is_critical cg (symbol fd) = CFDefExt (f fd)
      | otherwise = x
    process x = x
