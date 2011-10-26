module Ocram.Types 
-- export {{{1
(
    Ast
  , DebugSymbols
) where

-- import {{{1
import Language.C.Syntax.AST (CTranslUnit)

-- types {{{1
type Ast = CTranslUnit

type DebugSymbol = () -- TODO
type DebugSymbols = [DebugSymbol]
