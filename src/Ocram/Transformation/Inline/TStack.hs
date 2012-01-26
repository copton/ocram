{-# LANGUAGE TemplateHaskell #-}
module Ocram.Transformation.Inline.TStack
-- exports {{{1
(
  addTStacks
) where
-- imports {{{1
import Language.C.Syntax.AST
import Ocram.Analysis (start_functions, get_callees, dependency_list, CallGraph, is_start)
import Ocram.Query (local_variables, return_type)
import Ocram.Transformation.Inline.Names
import Ocram.Transformation.Inline.Types
import Ocram.Transformation.Util (un, ident)
import Ocram.Symbols (Symbol)
import Ocram.Types (Ast)
import Ocram.Util ((?:), fromJust_s, abort)
import qualified Data.Map as Map

addTStacks :: Transformation -- {{{1
addTStacks cg ast@(CTranslUnit decls ni) = do
  frames <- createTStackFrames cg ast
  let stacks = map createTStack $ start_functions cg
  return $ CTranslUnit (decls ++ frames ++ stacks) ni

createTStack :: Symbol -> CExtDecl
createTStack fName = CDeclExt (CDecl [CTypeSpec (CTypeDef (ident (frameType fName)) un)] [(Just (CDeclr (Just (ident (stackVar fName))) [] Nothing [] un), Nothing, Nothing)] un)

createTStackFrames :: CallGraph -> Ast -> WR [CExtDecl]
createTStackFrames cg ast =
  mapM (createTStackFrame cg ast) $ dependency_list cg

createTStackFrame :: CallGraph -> Ast -> Symbol -> WR CExtDecl
createTStackFrame cg ast name = do
  nestedFrames <- createNestedFramesUnion cg name
  return $ CDeclExt (CDecl [CStorageSpec (CTypedef un), CTypeSpec (CSUType (CStruct CStructTag Nothing (Just (
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

createNestedFramesUnion :: CallGraph -> Symbol -> WR (Maybe CDecl)
createNestedFramesUnion cg name =
  return $ if null entries then Nothing else Just createDecl
  where  
    createEntry sym = CDecl [CTypeSpec (CTypeDef (ident (frameType sym)) un)]
                      [(Just (CDeclr (Just (ident sym)) [] Nothing [] un), Nothing, Nothing)] un 
    entries = map createEntry $ $fromJust_s $ get_callees cg name
    createDecl = CDecl [CTypeSpec (CSUType (CStruct CUnionTag Nothing (Just entries) [] un) un)] [(Just (CDeclr (Just (ident frameUnion)) [] Nothing [] un), Nothing, Nothing)] un
