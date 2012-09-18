{-# LANGUAGE TemplateHaskell #-}
module Ocram.Backend.CreateTStacks
-- export {{{1
(
  create_tstacks
) where

-- import {{{1
import Ocram.Analysis (CallGraph)
import Language.C.Syntax.AST
import Language.C.Data.Node (undefNode, NodeInfo)
import Language.C.Data.Ident (internalIdent, Ident)
import Ocram.Util (fromJust_s, (?:))
import Ocram.Intermediate.Representation
import Ocram.Analysis (get_callees, is_start, dependency_list, start_functions)
import Ocram.Names (tframe, contVar, resVar, tstackVar, frameUnion)
import Ocram.Symbols (Symbol, symbol)
import Ocram.Query (return_type_fd)

import qualified Data.Map as M

create_tstacks :: CallGraph -> M.Map Symbol Function -> ([CDecl], [CDecl])
create_tstacks cg funs = (frames, stacks)
  where
    frames = map (tstackFrame cg . $fromJust_s . flip M.lookup funs) (dependency_list cg)
    stacks = map (tstackInstance . $fromJust_s . flip M.lookup funs) (start_functions cg)

tstackInstance :: Function -> CDecl
tstackInstance fun = stack
  where
    name = fun_name fun
    stack =
      CDecl [CTypeSpec (CTypeDef (ii (tframe name)) un)] [(Just (CDeclr (Just (ii (tstackVar name))) [] Nothing [] un), Nothing, Nothing)] un

tstackFrame :: CallGraph -> Function -> CDecl -- {{{1
tstackFrame cg fun = frame
  where
    name = fun_name fun

    frame = 
      CDecl [CTypeSpec (CTypeDef (ii (tframe name)) un)] [(Just (CDeclr (Just (ii name)) [] Nothing [] un), Nothing, Nothing)] un

    struct = CSUType (CStruct CStructTag Nothing (Just (
        continuation ?:
        result ?:
        nestedFrames ?:
        criticalVariables
      )) [] un) un

    continuation
      | is_start cg name = Nothing
      | otherwise        = Just $
          CDecl [CTypeSpec (CVoidType un)] [(Just (CDeclr (Just (ii contVar)) [CPtrDeclr [] un] Nothing [] un), Nothing, Nothing)] un

    result = case return_type_fd (fun_def fun) of
      (CVoidType _, []) -> Nothing
      (ts, dds)         -> Just $
          CDecl [CTypeSpec ts] [(Just (CDeclr (Just (ii resVar)) dds Nothing [] un), Nothing, Nothing)] un

    nestedFrames
      | null entries = Nothing
      | otherwise    = Just $
          CDecl [CTypeSpec (CSUType (CStruct CUnionTag Nothing (Just entries) [] un) un)] [(Just (CDeclr (Just (ii frameUnion)) [] Nothing [] un), Nothing, Nothing)] un
      where
        entries = map createEntry . $fromJust_s . get_callees cg . fun_name $ fun
        createEntry sym =
          CDecl [CTypeSpec (CTypeDef (ii (tframe sym)) un)] [(Just (CDeclr (Just (ii sym)) [] Nothing [] un), Nothing, Nothing)] un
      
    criticalVariables = map var_decl $ fun_cVars fun

un :: NodeInfo -- {{{2
un = undefNode

ii :: String -> Ident -- {{{2
ii = internalIdent
