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
import Ocram.Analysis (start_functions, critical_functions, call_order, get_callees, is_critical)
import Ocram.Query (function_info, FunctionInfo(FunctionInfo))
import Ocram.Transformation.Inline.Names
import Ocram.Transformation.Inline.Types
import Ocram.Transformation.Util (un, ident)
import Ocram.Symbols (Symbol)
import Ocram.Types (Ast)
import Ocram.Util ((?:), fromJust_s)
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as List

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
    fipairs <- createFiPairs ast
    frames <- mapM createTStackFrame $ List.reverse fipairs
    return frames

createFiPairs :: Ast -> WR [(Symbol, FunctionInfo)]
createFiPairs ast = do
  cg <- ask
  return $ fst $ foldl fld ([], Set.empty) $ concatMap (List.reverse . List.filter (is_critical cg) . $fromJust_s . call_order cg) $ Set.toList $ start_functions cg
  where
    fld (lst, set) fname
      | Set.member fname set = (lst, set)
      | otherwise = case function_info ast fname of
          Nothing -> (lst, set)
          (Just fi) -> ((fname, fi) : lst, Set.insert fname set)


createTStackFrame :: (Symbol, FunctionInfo) -> WR CExtDecl
createTStackFrame (name, FunctionInfo resultType _ vars _) = do
  nestedFrames <- createNestedFramesUnion name
  cg <- ask
  return $ CDeclExt (CDecl [CStorageSpec (CTypedef un), CTypeSpec (CSUType (CStruct CStructTag Nothing (Just (
       continuation (start_functions cg)
    ?: result resultType
    ?: nestedFrames
    ?: Map.elems vars
    )) [] un) un)] [(Just (CDeclr (Just (ident (frameType name))) [] Nothing [] un), Nothing, Nothing)] un)
  where
    result (CVoidType _) = Nothing
    result x = Just $ CDecl [CTypeSpec x] [(Just (CDeclr (Just (ident resVar)) [] Nothing [] un), Nothing, Nothing)] un
    continuation sf
      | Set.member name sf = Nothing
      | otherwise = Just (CDecl [CTypeSpec (CVoidType un)] [(Just (CDeclr (Just (ident contVar)) [CPtrDeclr [] un] Nothing [] un), Nothing, Nothing)] un)


createNestedFramesUnion :: Symbol -> WR (Maybe CDecl)
createNestedFramesUnion name = do
  cg <- ask
  let cf = critical_functions cg
  let createEntry sym = CDecl [CTypeSpec (CTypeDef (ident (frameType sym)) un)]
                      [(Just (CDeclr (Just (ident sym)) [] Nothing [] un), Nothing, Nothing)] un 
  let entries = map createEntry $ filter (flip Set.member cf) $ $fromJust_s $ get_callees cg name
  let createDecl = CDecl [CTypeSpec (CSUType (CStruct CUnionTag Nothing (Just entries) [] un) un)] [(Just (CDeclr (Just (ident frameUnion)) [] Nothing [] un), Nothing, Nothing)] un
  return $ if null entries then Nothing else Just createDecl
