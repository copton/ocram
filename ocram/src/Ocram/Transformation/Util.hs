module Ocram.Transformation.Util 
-- exports {{{1
(
  ident,
  map_critical_functions
) where

-- imports {{{1
import Language.C.Data.Ident (Ident(Ident))
import Language.C.Data.Node (undefNode)
import Language.C.Syntax.AST
import Ocram.Analysis (CallGraph, is_critical)
import Ocram.Symbols (symbol)

ident :: String -> Ident -- {{{1
ident s = Ident s 0 undefNode

map_critical_functions :: CallGraph -> CTranslationUnit a -> (CFunctionDef a -> CFunctionDef a) -> CTranslationUnit a -- {{{1
map_critical_functions cg (CTranslUnit ds ni) f = CTranslUnit (map process ds) ni
  where
    process x@(CFDefExt fd)
      | is_critical cg (symbol fd) = CFDefExt (f fd)
      | otherwise = x
    process x = x
