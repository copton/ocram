{-# LANGUAGE TemplateHaskell #-}
module Ocram.Backend.EStack
-- exports {{{1
(
  create_estacks
) where

-- imports {{{1
import Control.Monad ((<=<))
import Data.Maybe (mapMaybe)
import Ocram.Analysis (CallGraph, dependency_list, call_order, start_functions)
import Ocram.Intermediate (Function(..), fun_name, Variable(..))
import Language.C.Data.Ident (Ident, internalIdent)
import Language.C.Data.Node (undefNode, NodeInfo)
import Language.C.Syntax.AST
import Ocram.Symbols (Symbol)
import Ocram.Util (fromJust_s)
import Ocram.Names (estackVar, eframe)

import qualified Data.Map as M

create_estacks :: CallGraph -> M.Map Symbol Function -> ([CDecl], M.Map Symbol (Maybe CDecl)) -- {{{1
create_estacks cg cf = (frames, stacks)
  where
    frames = mapMaybe (estackFrame <=< flip M.lookup cf) (dependency_list cg)
    stacks = M.fromList $ map (estackInstance cg cf) (start_functions cg)

estackInstance :: CallGraph -> M.Map Symbol Function -> Symbol -> (Symbol, Maybe CDecl) -- {{{2
estackInstance cg cf fname
  | null frames = (fname, Nothing)
  | otherwise   = (fname, Just stack)
  where
    stack =
      CDecl [CTypeSpec (CSUType (CStruct CUnionTag Nothing (Just frames) [] un) un)] [(Just (CDeclr (Just (ii estackVar)) [] Nothing [] un), Nothing, Nothing)] un

    frames = mapMaybe frame . mapMaybe (flip M.lookup cf) . $fromJust_s . call_order cg $ fname

    frame callee
      | hasUncriticalVars callee = Just $
          CDecl [CTypeSpec (CTypeDef (ii (eframe (fun_name callee))) un)] [(Just (CDeclr (Just (ii (fun_name callee))) [] Nothing [] un), Nothing, Nothing)] un
      | otherwise                = Nothing 

estackFrame :: Function -> Maybe CDecl -- {{{2
estackFrame fun
  | hasUncriticalVars fun = Just frame
  | otherwise             = Nothing
  where
    name = fun_name fun
    frame =
      CDecl [CStorageSpec (CTypedef un), CTypeSpec (CSUType (CStruct CStructTag Nothing (Just uncriticalVariables) [] un) un)] [(Just (CDeclr (Just (ii (eframe name))) [] Nothing [] un), Nothing, Nothing)] un

    uncriticalVariables = map var_decl (fun_ncVars fun)

hasUncriticalVars :: Function -> Bool
hasUncriticalVars = not . null . fun_ncVars

un :: NodeInfo
un = undefNode

ii :: String -> Ident
ii = internalIdent
