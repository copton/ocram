module Ocram.Transformation.Inline.Finalize
-- exports {{{1
(
  finalize
) where

-- imports {{{1
import Language.C.Syntax.AST
import Ocram.Analysis (is_blocking, is_critical)
import Ocram.Symbols (symbol)
import Ocram.Transformation.Inline.Names
import Ocram.Transformation.Inline.Types
import Ocram.Transformation.Util (un, ident)

finalize :: Transformation -- {{{1
finalize cg (CTranslUnit ds ni) =
  return $ CTranslUnit (concatMap transform ds) ni
  where
    transform o@(CDeclExt cd)
      | is_blocking cg (symbol cd) = [CDeclExt (createBlockingFunctionDeclr cd)]
      | otherwise = [o]
    transform o@(CFDefExt fd)
      | is_critical cg (symbol fd) = []
      | otherwise = [o]
    transform o = [o]

-- createBlockingFunctionDeclr :: CDecl -> CDecl {{{2
createBlockingFunctionDeclr :: CDecl -> CDecl
createBlockingFunctionDeclr cd = let fName = symbol cd in
   CDecl [CTypeSpec (CVoidType un)] [(Just (CDeclr (Just (ident fName)) [CFunDeclr (Right ([CDecl [CTypeSpec (CTypeDef (ident (frameType fName)) un)] [(Just (CDeclr (Just (ident frameParam)) [CPtrDeclr [] un] Nothing [] un), Nothing, Nothing)] un], False)) [] un] Nothing [] un), Nothing, Nothing)] un
