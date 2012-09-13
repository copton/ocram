{-# LANGUAGE TemplateHaskell #-}
module Ocram.Intermediate 
-- exports {{{1
(
    module Ocram.Intermediate.Representation
  , ast_2_ir
) where

-- imports {{{1
import Control.Monad.Writer (runWriter, Writer, tell)
import Ocram.Intermediate.Representation
import Ocram.Intermediate.BooleanShortCircuiting
import Ocram.Intermediate.BuildBasicBlocks
import Ocram.Intermediate.Cleanup
import Ocram.Intermediate.CollectDeclarations
import Ocram.Intermediate.DesugarControlStructures
import Ocram.Intermediate.NormalizeCriticalCalls
import Ocram.Symbols (Symbol)
import Language.C.Syntax.AST

import qualified Data.Map as M

ast_2_ir :: M.Map Symbol CFunDef -> M.Map Symbol Function -- {{{1
ast_2_ir cf = M.map convert cf
  where
    cf' = M.keysSet cf

    convert :: CFunDef -> Function
    convert fd =
      let (body, vars) = runWriter $ process fd in
      Function vars fd body

    process fd = 
          return fd
      >>= wrap     collect_declarations
      >>= return . desugar_control_structures
      >>= wrap     (boolean_short_circuiting cf')
      >>= return . cleanup
      >>= wrap     (normalize_critical_calls cf)
      >>= return . build_basic_blocks cf'

    wrap :: (a -> (b, [Variable])) -> (a -> Writer [Variable] b)
    wrap f x = do
      let (y, vars) = f x
      tell vars
      return y
