{-# LANGUAGE TemplateHaskell #-}
module Ocram.Transformation.Translate.Internal
where
-- imports {{{1
import Language.C.Syntax.AST
import Ocram.Analysis (CallGraph, dependency_list, start_functions, is_start, get_callees, is_blocking, is_critical)
import Ocram.Debug (un)
import Ocram.Query (is_blocking_function', return_type, local_variables, is_function_declaration)
import Ocram.Symbols (symbol)
import Ocram.Transformation.Names
import Ocram.Transformation.Types
import Ocram.Transformation.Util (ident)
import Ocram.Util ((?:), fromJust_s, abort)

import qualified Data.Map as Map

add_blocking_function_decls :: CTranslUnit' -> CTranslUnit' -- {{{1
add_blocking_function_decls (CTranslUnit ds ni) = CTranslUnit (ds ++ extraDs) ni
  where
    extraDs = foldr proc [] ds
    proc (CDeclExt cd) cds
      | is_blocking_function' cd = CDeclExt (createBlockingFunctionDeclr cd) : cds
      | otherwise = cds
    proc _ cds = cds

    createBlockingFunctionDeclr cd = decl
      where
      decl = CDecl ts [(Just declr, Nothing, Nothing)] un
      ts = [CTypeSpec (CVoidType un)]    
      declr = CDeclr iden fdeclr Nothing [] un
      iden = Just (ident fName)
      fName = symbol cd
      fdeclr = [CFunDeclr (Right ([param], False)) [] un]
      param = CDecl ts' [(Just declr', Nothing, Nothing)] un
      ts' = [CTypeSpec (CTypeDef (ident (frameType fName)) un)]
      declr' = CDeclr Nothing [CPtrDeclr [] un] Nothing [] un

remove_critical_functions :: CallGraph -> CTranslUnit' -> CTranslUnit' -- {{{1
remove_critical_functions cg (CTranslUnit ds ni) = (CTranslUnit (foldr proc [] ds) ni)
  where
    proc o@(CDeclExt cd) cds
      | is_blocking_function' cd = cds
      | is_function_declaration cd && is_critical cg (symbol cd) && not (is_blocking cg (symbol cd)) = cds
      | otherwise = o : cds
    proc o@(CFDefExt fd) cds
      | is_critical cg (symbol fd) = cds
      | otherwise = o : cds
    proc o cds = o : cds

add_tstacks :: CallGraph -> CTranslUnit' -> CTranslUnit'
add_tstacks cg ast@(CTranslUnit decls ni) =
  CTranslUnit (decls ++ frames ++ stacks) ni
  where
    frames = map createTStackFrame $ dependency_list cg
    stacks = map createTStack $ start_functions cg

    createTStack fName = CDeclExt (CDecl [CTypeSpec (CTypeDef (ident (frameType fName)) un)] [(Just (CDeclr (Just (ident (stackVar fName))) [] Nothing [] un), Nothing, Nothing)] un)

    createTStackFrame name =
      let nestedFrames = createNestedFramesUnion name in
      CDeclExt (CDecl [CStorageSpec (CTypedef un), CTypeSpec (CSUType (CStruct CStructTag Nothing (Just (
        continuation ?:
        result ($fromJust_s (return_type ast name)) ?:
        nestedFrames ?:
        localVariables
        )) [] un) un)] [(Just (CDeclr (Just (ident (frameType name))) [] Nothing [] un), Nothing, Nothing)] un)
      where
        result (CVoidType _, []) = Nothing
        result (ts, dds) = Just $ CDecl [CTypeSpec ts] [(Just (CDeclr (Just (ident resVar)) dds Nothing [] un), Nothing, Nothing)] un
        continuation
          | is_start cg name = Nothing
          | otherwise = Just $ CDecl [CTypeSpec (CVoidType un)] [(Just (CDeclr (Just (ident contVar)) [CPtrDeclr [] un] Nothing [] un), Nothing, Nothing)] un
        localVariables = map removeInit $ Map.elems ($fromJust_s (local_variables ast name))
        removeInit (CDecl x1 [(x2, _, x3)] x4) = CDecl x1 [(x2, Nothing, x3)] x4
        removeInit _ = $abort "unexpected parameters"

    createNestedFramesUnion name =
      if null entries then Nothing else Just createDecl
      where  
        createEntry sym = CDecl [CTypeSpec (CTypeDef (ident (frameType sym)) un)]
                          [(Just (CDeclr (Just (ident sym)) [] Nothing [] un), Nothing, Nothing)] un 
        entries = map createEntry $ $fromJust_s $ get_callees cg name
        createDecl = CDecl [CTypeSpec (CSUType (CStruct CUnionTag Nothing (Just entries) [] un) un)] [(Just (CDeclr (Just (ident frameUnion)) [] Nothing [] un), Nothing, Nothing)] un


