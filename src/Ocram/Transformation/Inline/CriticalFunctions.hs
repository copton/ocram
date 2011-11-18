module Ocram.Transformation.Inline.CriticalFunctions
-- exports {{{1
(
  addBlockingFunctionDecls, removeCriticalFunctions
) where

-- imports {{{1
import Language.C.Syntax.AST
import Ocram.Analysis (is_blocking, is_critical)
import Ocram.Query (is_blocking_function')
import Ocram.Symbols (symbol)
import Ocram.Transformation.Inline.Names
import Ocram.Transformation.Inline.Types
import Ocram.Transformation.Util (un, ident)

addBlockingFunctionDecls :: Transformation -- {{{1
addBlockingFunctionDecls cg (CTranslUnit ds ni) = return $ CTranslUnit (ds ++ extraDs) ni
  where
    extraDs = foldr proc [] ds
    proc cd cds
      | is_blocking cg (symbol cd) = CDeclExt (createBlockingFunctionDeclr cd) : cds
      | otherwise = cds

    createBlockingFunctionDeclr cd = let fName = symbol cd in
       CDecl [CTypeSpec (CVoidType un)] [(Just (CDeclr (Just (ident fName)) [CFunDeclr (Right ([CDecl [CTypeSpec (CTypeDef (ident (frameType fName)) un)] [(Just (CDeclr (Just (ident frameParam)) [CPtrDeclr [] un] Nothing [] un), Nothing, Nothing)] un], False)) [] un] Nothing [] un), Nothing, Nothing)] un

removeCriticalFunctions :: Transformation -- {{{1
removeCriticalFunctions cg (CTranslUnit ds ni) = return (CTranslUnit (foldr proc [] ds) ni)
  where
    proc o@(CDeclExt cd) cds
      | is_blocking_function' cd = cds
      | is_critical cg (symbol cd) && not (is_blocking cg (symbol cd)) = cds
      | otherwise = o : cds
    proc o@(CFDefExt fd) cds
      | is_critical cg (symbol fd) = cds
      | otherwise = o : cds
    proc o cds = o : cds
