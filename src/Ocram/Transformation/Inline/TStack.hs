{-# LANGUAGE TemplateHaskell #-}
-- add tstack structures and variables
module Ocram.Transformation.Inline.TStack
-- exports {{{1
(
  addTStacks
) where
-- imports {{{1
import Control.Monad.Reader (ask)
import Language.C.Syntax.AST
import Ocram.Analysis (start_functions, critical_functions, get_callees, critical_function_dependency_list)
import Ocram.Query (local_variables, return_type)
import Ocram.Transformation.Inline.Names
import Ocram.Transformation.Inline.Types
import Ocram.Transformation.Util (un, ident)
import Ocram.Symbols (Symbol)
import Ocram.Types (Ast)
import Ocram.Util ((?:), fromJust_s, abort)
import qualified Data.Set as Set
import qualified Data.Map as Map

addTStacks :: Ast -> WR Ast -- {{{1
addTStacks ast@(CTranslUnit decls ni) = do
  frames <- createTStackFrames ast
  cg <- ask
  let stacks = map createTStack $ Set.toList $ start_functions cg
  return $ CTranslUnit (frames ++ stacks ++ decls) ni

createTStack :: Symbol -> CExtDecl
createTStack fName = CDeclExt (CDecl [CTypeSpec (CTypeDef (ident (frameType fName)) un)] [(Just (CDeclr (Just (ident (stackVar fName))) [] Nothing [] un), Nothing, Nothing)] un)

createTStackFrames :: Ast -> WR [CExtDecl]
createTStackFrames ast = do
  cg <- ask
  mapM (createTStackFrame ast) $ critical_function_dependency_list cg

createTStackFrame :: Ast -> Symbol -> WR CExtDecl
createTStackFrame ast name = do
  nestedFrames <- createNestedFramesUnion name
  cg <- ask
  return $ CDeclExt (CDecl [CStorageSpec (CTypedef un), CTypeSpec (CSUType (CStruct CStructTag Nothing (Just (
    continuation (start_functions cg) ?:
    result ($fromJust_s (return_type ast name)) ?:
    nestedFrames ?:
    localVariables
    )) [] un) un)] [(Just (CDeclr (Just (ident (frameType name))) [] Nothing [] un), Nothing, Nothing)] un)
  where
    result (CVoidType _) = Nothing
    result x = Just $ CDecl [CTypeSpec x] [(Just (CDeclr (Just (ident resVar)) [] Nothing [] un), Nothing, Nothing)] un
    continuation sf
      | Set.member name sf = Nothing
      | otherwise = Just (CDecl [CTypeSpec (CVoidType un)] [(Just (CDeclr (Just (ident contVar)) [CPtrDeclr [] un] Nothing [] un), Nothing, Nothing)] un)
    localVariables = map removeInit $ Map.elems ($fromJust_s (local_variables ast name))
    removeInit (CDecl x1 [(x2, _, x3)] x4) = CDecl x1 [(x2, Nothing, x3)] x4
    removeInit _ = $abort "unexpected parameters"

createNestedFramesUnion :: Symbol -> WR (Maybe CDecl)
createNestedFramesUnion name = do
  cg <- ask
  let cf = critical_functions cg
  let createEntry sym = CDecl [CTypeSpec (CTypeDef (ident (frameType sym)) un)]
                      [(Just (CDeclr (Just (ident sym)) [] Nothing [] un), Nothing, Nothing)] un 
  let entries = map createEntry $ filter (flip Set.member cf) $ $fromJust_s $ get_callees cg name
  let createDecl = CDecl [CTypeSpec (CSUType (CStruct CUnionTag Nothing (Just entries) [] un) un)] [(Just (CDeclr (Just (ident frameUnion)) [] Nothing [] un), Nothing, Nothing)] un
  return $ if null entries then Nothing else Just createDecl
