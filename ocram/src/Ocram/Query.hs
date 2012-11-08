{-# LANGUAGE TemplateHaskell #-}
module Ocram.Query
-- export {{{1
(
    function_definition
  , is_function_declaration
  , function_parameters_fd, function_parameters_cd
  , return_type_fd, return_type_cd, object_type
  , is_blocking_function, is_start_function
) where

-- import {{{1
import Data.Maybe (mapMaybe)
import Language.C.Data.Node (undefNode)
import Language.C.Data.Ident (Ident(Ident))
import Language.C.Syntax.AST
import Ocram.Names (blockingAttr, startAttr)
import Ocram.Symbols (Symbol, symbol)
import Ocram.Util (abort, unexp)
import qualified Data.List as List

function_definition :: CTranslationUnit a -> Symbol -> Maybe (CFunctionDef a) -- {{{1
function_definition (CTranslUnit eds _) name =
  List.find ((==name) . symbol) $ mapMaybe functionDefinition eds

is_blocking_function :: CDeclaration a -> Bool -- {{{1
is_blocking_function (CDecl ss [(Just (CDeclr _ ((CFunDeclr _ _ _):_) _ _ _), Nothing, Nothing)] _) = any isBlockingAttribute ss
is_blocking_function _ = False

is_start_function :: CFunctionDef a -> Bool -- {{{1
is_start_function (CFunDef specs _ _ _ _) = any isStartAttr specs 

function_parameters_fd :: CFunctionDef a -> [CDeclaration a] -- {{{1
function_parameters_fd (CFunDef _ (CDeclr _ [cfd] _ _ _) _ _ _) = functionParameters cfd
function_parameters_fd x = $abort $ unexp x

function_parameters_cd :: CDeclaration a -> [CDeclaration a] --- {{{1
function_parameters_cd (CDecl _ [(Just (CDeclr _ (cfd:_) _ _ _), Nothing, Nothing)] _) = functionParameters cfd
function_parameters_cd x = $abort $ unexp x


return_type_fd :: CFunctionDef a -> (CTypeSpecifier a, [CDerivedDeclarator a]) -- {{{1
return_type_fd (CFunDef tss (CDeclr _ ((CFunDeclr _ _ _):dds)_ _ _) _ _ _) = (extractTypeSpec tss, dds)
return_type_fd x = $abort $ unexp x

return_type_cd :: CDeclaration a -> (CTypeSpecifier a, [CDerivedDeclarator a]) -- {{{1
return_type_cd (CDecl tss [(Just (CDeclr _ ((CFunDeclr _ _ _):dds) _ _ _), _, _)] _) = (extractTypeSpec tss, dds)
return_type_cd x = $abort $ unexp x

is_function_declaration :: CDeclaration a -> Bool -- {{{1
is_function_declaration (CDecl _ [(Just (CDeclr _ dds _ _ _), _, _)] _) = any isFunDeclr dds
  where
    isFunDeclr (CFunDeclr _ _ _) = True
    isFunDeclr _ = False
is_function_declaration _ = False

object_type :: CDeclaration a -> CTypeSpecifier a -- {{{1
object_type (CDecl tss _ _) = extractTypeSpec tss

-- utils {{{1

isBlockingAttribute :: CDeclarationSpecifier a -> Bool
isBlockingAttribute (CTypeQual (CAttrQual (CAttr (Ident attr _ _) [] _))) = attr == blockingAttr
isBlockingAttribute _ = False

isStartAttr :: CDeclarationSpecifier a -> Bool
isStartAttr (CTypeQual (CAttrQual (CAttr (Ident attr _ _) [] _))) = attr == startAttr
isStartAttr _ = False

functionDeclaration :: CExternalDeclaration a -> Maybe (CDeclaration a)
functionDeclaration (CDeclExt x)
  | is_function_declaration x = Just x
  | otherwise = Nothing
functionDeclaration _ = Nothing

functionDefinition :: CExternalDeclaration a -> Maybe (CFunctionDef a)
functionDefinition (CFDefExt x) = Just x
functionDefinition _ = Nothing

functionParameters :: CDerivedDeclarator a -> [CDeclaration a]
functionParameters (CFunDeclr (Right (ps, _)) _ _) = ps
functionParameters x = $abort $ unexp $ CDeclr Nothing [fmap (const undefNode) x] Nothing [] undefNode -- there is no instance Pretty CDerivedDeclr

extractTypeSpec :: [CDeclarationSpecifier a] -> CTypeSpecifier a
extractTypeSpec [] = $abort "type specifier expected"
extractTypeSpec (CTypeSpec ts:_) = ts
extractTypeSpec (_:xs) = extractTypeSpec xs
