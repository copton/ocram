{-# LANGUAGE TemplateHaskell #-}
module Ocram.Intermediate 
-- exports {{{1
(
    module Ocram.Intermediate.Representation
  , ast_2_ir
) where

-- imports {{{1
import Control.Monad.Writer (runWriter, Writer, tell)
import Data.Either (partitionEithers)
import Ocram.Intermediate.Representation
import Ocram.Intermediate.BooleanShortCircuiting
import Ocram.Intermediate.BuildBasicBlocks
import Ocram.Intermediate.SequencializeBody
import Ocram.Intermediate.CollectDeclarations
import Ocram.Intermediate.DesugarControlStructures
import Ocram.Intermediate.NormalizeCriticalCalls
import Ocram.Intermediate.CriticalVariables
import Ocram.Symbols (Symbol)
import Ocram.Query (return_type_fd, return_type_cd)
import Language.C.Syntax.AST

import qualified Data.Map as M
import qualified Data.Set as S

ast_2_ir :: M.Map Symbol CDecl -> M.Map Symbol CFunDef -> M.Map Symbol Function -- {{{1
ast_2_ir bf cf = M.map (critical_variables . convert) cf
  where
    sf  = M.keysSet cf `S.union` M.keysSet bf
    sf' = M.map return_type_fd cf `M.union` M.map return_type_cd bf

    convert :: CFunDef -> Function
    convert fd =
      let
        (items, vars) = runWriter $ process fd 
        (autoVars, staticVars) = partitionEithers vars
        (entry, body) = build_basic_blocks sf items
      in
        Function autoVars [] staticVars fd body entry

    process fd = 
          return fd
      >>= wrap'    collect_declarations
      >>= return . desugar_control_structures
      >>= wrap     (boolean_short_circuiting sf)
      >>= return . sequencialize_body
      >>= wrap     (normalize_critical_calls sf')

    wrap f = wrap' (\x -> let (y, autoVars) = f x in (y, autoVars, []))

    wrap' f x = do
      let (y, autoVars, staticVars) = f x
      tell $ map Left autoVars
      tell $ map Right staticVars
      return y