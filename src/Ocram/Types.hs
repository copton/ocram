module Ocram.Types 
-- export {{{1
(
    Ast
  , DebugSymbols
) where

-- import {{{1
import Language.C.Syntax.AST (CTranslUnit)

type Ast = CTranslUnit -- {{{1

type DebugSymbol = () -- TODO -- {{{1
type DebugSymbols = [DebugSymbol]
