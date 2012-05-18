{-# LANGUAGE TemplateHaskell #-}
module Ocram.Query
-- export {{{1
(
    function_definition, function_declaration
  , is_function_declaration
  , function_parameters,  return_type, local_variables
  , function_parameters_fd, function_parameters_cd
  , return_type_fd, return_type_cd
  , local_variables_fd, local_variables_cd
  , is_blocking_function, is_start_function
  , is_blocking_function', is_start_function'
  , SymbolTable
) where

-- import {{{1
import Data.Data (Data)
import Data.Generics (everything, mkQ, extQ)
import Data.Maybe (mapMaybe)
import Language.C.Data.Node (undefNode)
import Language.C.Data.Ident (Ident(Ident))
import Language.C.Syntax.AST
import Ocram.Names (blockingAttr, startAttr)
import Ocram.Symbols (Symbol, symbol)
import Ocram.Util (abort, unexp)
import qualified Data.List as List
import qualified Data.Map as Map

type SymbolTable a = Map.Map Symbol (CDeclaration a) -- {{{1

function_declaration :: CTranslationUnit a -> Symbol -> Maybe (CDeclaration a) -- {{{1
function_declaration (CTranslUnit eds _) name =
  List.find ((==name) . symbol) $ mapMaybe functionDeclaration eds

function_definition :: CTranslationUnit a -> Symbol -> Maybe (CFunctionDef a) -- {{{1
function_definition (CTranslUnit eds _) name =
  List.find ((==name) . symbol) $ mapMaybe functionDefinition eds

is_blocking_function :: CTranslationUnit a -> Symbol -> Bool -- {{{1
is_blocking_function ast name =
  maybe False is_blocking_function' $ function_declaration ast name

is_blocking_function' :: CDeclaration a -> Bool -- {{{1
is_blocking_function' (CDecl ss [(Just (CDeclr _ ((CFunDeclr _ _ _):_) _ _ _), Nothing, Nothing)] _) = any isBlockingAttribute ss
is_blocking_function' _ = False

is_start_function' :: CFunctionDef a -> Bool -- {{{1
is_start_function' (CFunDef specs _ _ _ _) = any isStartAttr specs 

is_start_function :: CTranslationUnit a -> Symbol -> Bool -- {{{1
is_start_function ast name =
  maybe False is_start_function' $ function_definition ast name

function_parameters :: CTranslationUnit a -> Symbol -> Maybe [CDeclaration a] -- {{{1
function_parameters = apply function_parameters_fd function_parameters_cd

function_parameters_fd :: CFunctionDef a -> [CDeclaration a] -- {{{1
function_parameters_fd (CFunDef _ (CDeclr _ [cfd] _ _ _) _ _ _) = functionParameters cfd
function_parameters_fd x = $abort $ unexp x

function_parameters_cd :: CDeclaration a -> [CDeclaration a] --- {{{1
function_parameters_cd (CDecl _ [(Just (CDeclr _ (cfd:_) _ _ _), Nothing, Nothing)] _) = functionParameters cfd
function_parameters_cd x = $abort $ unexp x

return_type :: CTranslationUnit a -> Symbol -> Maybe (CTypeSpecifier a, [CDerivedDeclarator a]) -- {{{1
return_type = apply return_type_fd return_type_cd

return_type_fd :: CFunctionDef a -> (CTypeSpecifier a, [CDerivedDeclarator a]) -- {{{1
return_type_fd (CFunDef tss (CDeclr _ ((CFunDeclr _ _ _):dds)_ _ _) _ _ _) = (extractTypeSpec tss, dds)
return_type_fd x = $abort $ unexp x

return_type_cd :: CDeclaration a -> (CTypeSpecifier a, [CDerivedDeclarator a]) -- {{{1
return_type_cd (CDecl tss [(Just (CDeclr _ ((CFunDeclr _ _ _):dds) _ _ _), _, _)] _) = (extractTypeSpec tss, dds)
return_type_cd x = $abort $ unexp x

local_variables :: Data a => CTranslationUnit a -> Symbol -> Maybe (SymbolTable a) -- {{{1
local_variables = apply local_variables_fd local_variables_cd
 
local_variables_fd :: Data a => CFunctionDef a -> SymbolTable a -- {{{1
local_variables_fd fd = foldl addDecls Map.empty ds
  where
    ds = function_parameters_fd fd ++ query
    query = everything (++) (mkQ [] queryBlockItem `extQ` queryCExp) fd
    queryBlockItem (CBlockDecl cd@(CDecl ds' _ _))
      | any isStatic ds' = []
      | otherwise = [cd]
      where
      isStatic (CStorageSpec (CStatic _)) = True
      isStatic _ = False
    queryBlockItem _ = []
    queryCExp (CFor (Right cd) _ _ _ _) = [cd]
    queryCExp _ = []

local_variables_cd :: CDeclaration a -> SymbolTable a -- {{{1
local_variables_cd cd = foldl addDecls Map.empty $ function_parameters_cd cd

is_function_declaration :: CDeclaration a -> Bool -- {{{1
is_function_declaration (CDecl _ [(Just (CDeclr _ dds _ _ _), _, _)] _) = any isFunDeclr dds
  where
    isFunDeclr (CFunDeclr _ _ _) = True
    isFunDeclr _ = False
is_function_declaration _ = False

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

apply :: (CFunctionDef a -> b) -> (CDeclaration a -> b) -> CTranslationUnit a -> Symbol -> Maybe b
apply fdf cdf ast name =
  case function_definition ast name of
    Just fd -> Just $ fdf fd
    Nothing -> case function_declaration ast name of
      Just cd -> Just $ cdf cd
      Nothing -> Nothing

addDecls :: SymbolTable a -> CDeclaration a -> SymbolTable a
addDecls st cd = Map.insert (symbol cd) cd st
