-- add tstack structures and variables
module Ocram.Transformation.Inline.Step2
-- exports {{{1
(
  step2
) where
-- imports {{{1
import Control.Monad.Reader (ask)
import Data.Maybe (fromJust)
import Language.C.Data.Ident
import Language.C.Syntax.AST
import Ocram.Analysis (start_functions, critical_functions, call_order, callees)
import Ocram.Transformation.Inline.FunctionInfo (function_info)
import Ocram.Transformation.Inline.Names
import Ocram.Transformation.Inline.Types
import Ocram.Transformation.Util (un, ident)
import Ocram.Symbols (Symbol)
import Ocram.Types (Ast)
import Ocram.Util ((?:))
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as List

-- step2 :: FunctionInfos -> Ast -> WR Ast {{{1
step2 :: Ast -> WR Ast
step2 (CTranslUnit decls ni) = do
  frames <- createTStackFrames
  cg <- ask
  let stacks = map createTStack $ Set.toList $ start_functions cg
  return $ CTranslUnit (frames ++ stacks ++ decls) ni

-- createTStack :: Symbol -> CExtDecl {{{2
createTStack :: Symbol -> CExtDecl
createTStack fName = CDeclExt (CDecl [CTypeSpec (CTypeDef (ident (frameType fName)) un)] [(Just (CDeclr (Just (ident (stackVar fName))) [] Nothing [] un), Nothing, Nothing)] un)

-- createTStackFrames :: WR [CExtDecl] {{{2
createTStackFrames :: WR [CExtDecl]
createTStackFrames = do
    fipairs <- createFiPairs
    frames <- mapM createTStackFrame $ List.reverse fipairs
    return frames

createFiPairs :: WR [(Symbol, FunctionInfo)]
createFiPairs = do
  cg <- ask
  return $ fst $ foldl fld ([], Set.empty) $ concatMap (List.reverse . fromJust . call_order cg) $ Set.toList $ start_functions cg
  where
    fld (lst, set) fname
      | Set.member fname set = (lst, set)
      | otherwise = case function_info fname of
          Nothing -> (lst, set)
          (Just fi) -> ((fname, fi) : lst, Set.insert fname set)


createTStackFrame :: (Symbol, FunctionInfo) -> WR CExtDecl
createTStackFrame (name, fi@(FunctionInfo resultType _ vars _)) = do
  nestedFrames <- createNestedFramesUnion (name, fi)
  cg <- ask
  let sf = start_functions cg
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


createNestedFramesUnion :: (Symbol, FunctionInfo) -> WR (Maybe CDecl)
createNestedFramesUnion (name, fi) = do
  cg <- ask
  let cf = critical_functions cg
  let createEntry sym = CDecl [CTypeSpec (CTypeDef (ident (frameType sym)) un)]
                      [(Just (CDeclr (Just (ident sym)) [] Nothing [] un), Nothing, Nothing)] un 
  let entries = map createEntry $ filter (flip Set.member cf) $ fromJust $ callees cg name
  let createDecl = CDecl [CTypeSpec (CSUType (CStruct CUnionTag Nothing (Just entries) [] un) un)] [(Just (CDeclr (Just (ident frameUnion)) [] Nothing [] un), Nothing, Nothing)] un
  return $ if null entries then Nothing else Just createDecl
