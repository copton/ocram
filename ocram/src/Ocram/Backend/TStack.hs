{-# LANGUAGE TemplateHaskell #-}
module Ocram.Backend.TStack
-- export {{{1
(
  create_tstacks
) where

-- import {{{1
import Data.List (nub)
import Language.C.Data.Ident (internalIdent, Ident)
import Language.C.Data.Node (undefNode, NodeInfo)
import Language.C.Syntax.AST
import Ocram.Analysis (CallGraph)
import Ocram.Analysis (get_callees, is_start, dependency_list, start_functions)
import Ocram.Intermediate (Function(..), fun_name, Variable(..))
import Ocram.Names (tframe, contVar, resVar, tstackVar, frameUnion)
import Ocram.Query (return_type_fd, return_type_cd, function_parameters_cd)
import Ocram.Symbols (Symbol)
import Ocram.Util (abort, fromJust_s, (?:))

import qualified Data.Map as M

create_tstacks :: CallGraph -> M.Map Symbol CDecl -> M.Map Symbol Function -> ([(Symbol, CDecl)], [CDecl]) -- {{{1
create_tstacks cg bf cf = (frames, stacks)
  where
    frames = map frame (dependency_list cg)
    stacks = map (tstackInstance . $fromJust_s . flip M.lookup cf) (start_functions cg)

    frame fname = case M.lookup fname cf of
      Nothing -> case M.lookup fname bf of
        Nothing -> $abort "non-critical function in dependency list?"
        Just decl -> (fname, tstackFrame cg (return_type_cd decl) fname (function_parameters_cd decl))
      Just fun -> (fname, tstackFrame cg (return_type_fd (fun_def fun)) fname (map var_decl (fun_cVars fun)))

tstackInstance :: Function -> CDecl -- {{{2
tstackInstance fun = stack
  where
    name = fun_name fun
    stack =
      CDecl [CTypeSpec (CTypeDef (ii (tframe name)) un)] [(Just (CDeclr (Just (ii (tstackVar name))) [] Nothing [] un), Nothing, Nothing)] un

tstackFrame :: CallGraph -> (CTypeSpec, [CDerivedDeclr]) -> Symbol -> [CDecl] -> CDecl -- {{{2
tstackFrame cg returnType name cvars = frame
  where
    frame = 
      CDecl [CStorageSpec (CTypedef un), CTypeSpec struct] [(Just (CDeclr (Just (ii (tframe name))) [] Nothing [] un), Nothing, Nothing)] un

    struct = CSUType (CStruct CStructTag Nothing (Just (
        continuation ?:
        result ?:
        nestedFrames ?:
        cvars
      )) [] un) un

    continuation
      | is_start cg name = Nothing
      | otherwise    = Just $
          CDecl [CTypeSpec (CVoidType un)] [(Just (CDeclr (Just (ii contVar)) [CPtrDeclr [] un] Nothing [] un), Nothing, Nothing)] un

    result = case returnType of
      (CVoidType _, []) -> Nothing
      (ts, dds)         -> Just $
          CDecl [CTypeSpec ts] [(Just (CDeclr (Just (ii resVar)) dds Nothing [] un), Nothing, Nothing)] un

    nestedFrames
      | null entries = Nothing
      | otherwise    = Just $
          CDecl [CTypeSpec (CSUType (CStruct CUnionTag Nothing (Just entries) [] un) un)] [(Just (CDeclr (Just (ii frameUnion)) [] Nothing [] un), Nothing, Nothing)] un
      where
        entries = map createEntry $ nub $ map fst $ get_callees cg name
        createEntry sym =
          CDecl [CTypeSpec (CTypeDef (ii (tframe sym)) un)] [(Just (CDeclr (Just (ii sym)) [] Nothing [] un), Nothing, Nothing)] un

un :: NodeInfo -- {{{2
un = undefNode

ii :: String -> Ident -- {{{2
ii = internalIdent
