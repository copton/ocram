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
import Data.Generics (everything, mkQ, extQ)
import Data.Maybe (mapMaybe)
import Language.C.Data.Ident (Ident(Ident))
import Language.C.Syntax.AST
import Ocram.Names (blockingAttr, startAttr)
import Ocram.Symbols (Symbol, symbol)
import Ocram.Types (Ast)
import Ocram.Util (abort)
import qualified Data.List as List
import qualified Data.Map as Map
type SymbolTable = Map.Map Symbol CDecl -- {{{1

function_declaration :: Ast -> Symbol -> Maybe CDecl -- {{{1
function_declaration (CTranslUnit eds _) name =
  List.find ((==name) . symbol) $ mapMaybe functionDeclaration eds

function_definition :: Ast -> Symbol -> Maybe CFunDef -- {{{1
function_definition (CTranslUnit eds _) name =
  List.find ((==name) . symbol) $ mapMaybe functionDefinition eds

is_blocking_function :: Ast -> Symbol -> Bool -- {{{1
is_blocking_function ast name =
  maybe False is_blocking_function' $ function_declaration ast name

is_blocking_function' :: CDecl -> Bool -- {{{1
is_blocking_function' (CDecl ss [(Just (CDeclr _ [CFunDeclr _ _ _] Nothing _ _), Nothing, Nothing)] _) = any isBlockingAttribute ss
is_blocking_function' _ = False

is_start_function' :: CFunDef -> Bool -- {{{1
is_start_function' (CFunDef specs _ _ _ _) = any isStartAttr specs 

is_start_function :: Ast -> Symbol -> Bool -- {{{1
is_start_function ast name =
  maybe False is_start_function' $ function_definition ast name

function_parameters :: Ast -> Symbol -> Maybe [CDecl] -- {{{1
function_parameters = apply function_parameters_fd function_parameters_cd

function_parameters_fd :: CFunDef -> [CDecl] -- {{{1
function_parameters_fd (CFunDef _ (CDeclr _ [cfd] _ _ _) _ _ _) = functionParameters cfd
function_parameters_fd _ = $abort "unexpected parameters" 

function_parameters_cd :: CDecl -> [CDecl] --- {{{1
function_parameters_cd (CDecl _ [(Just (CDeclr _ [cfd] _ _ _), Nothing, Nothing)] _) = functionParameters cfd
function_parameters_cd _ = $abort "unexpected parameters" 

return_type :: Ast -> Symbol -> Maybe CTypeSpec -- {{{1
return_type = apply return_type_fd return_type_cd

return_type_fd :: CFunDef -> CTypeSpec -- {{{1
return_type_fd (CFunDef tss _ _ _ _) = extractTypeSpec tss

return_type_cd :: CDecl -> CTypeSpec -- {{{1
return_type_cd (CDecl tss _ _) = extractTypeSpec tss

local_variables :: Ast -> Symbol -> Maybe SymbolTable -- {{{1
local_variables = apply local_variables_fd local_variables_cd
 
local_variables_fd :: CFunDef -> SymbolTable -- {{{1
local_variables_fd fd = foldl addDecls Map.empty ds
  where
    ds = function_parameters_fd fd ++ query
    query = everything (++) (mkQ [] queryBlockItem `extQ` queryCExp) fd
    queryBlockItem (CBlockDecl cd) = [cd]
    queryBlockItem _ = []
    queryCExp (CFor (Right cd) _ _ _ _) = [cd]
    queryCExp _ = []

local_variables_cd :: CDecl -> SymbolTable -- {{{1
local_variables_cd cd = foldl addDecls Map.empty $ function_parameters_cd cd

is_function_declaration :: CDecl -> Bool -- {{{1
is_function_declaration (CDecl _ [(Just (CDeclr _ dds _ _ _), _, _)] _) = any isFunDeclr dds
  where
    isFunDeclr (CFunDeclr _ _ _) = True
    isFunDeclr _ = False
is_function_declaration _ = False

-- utils {{{1

isBlockingAttribute :: CDeclSpec -> Bool
isBlockingAttribute (CTypeQual (CAttrQual (CAttr (Ident attr _ _) [] _))) = attr == blockingAttr
isBlockingAttribute _ = False

isStartAttr :: CDeclSpec -> Bool
isStartAttr (CTypeQual (CAttrQual (CAttr (Ident attr _ _) [] _))) = attr == startAttr
isStartAttr _ = False

functionDeclaration :: CExtDecl -> Maybe CDecl
functionDeclaration (CDeclExt x)
  | is_function_declaration x = Just x
  | otherwise = Nothing
functionDeclaration _ = Nothing

functionDefinition :: CExtDecl -> Maybe CFunDef
functionDefinition (CFDefExt x) = Just x
functionDefinition _ = Nothing

functionParameters :: CDerivedDeclr -> [CDecl]
functionParameters (CFunDeclr (Right (ps, False)) _ _) = ps
functionParameters _ = $abort "unexpected parameters"

extractTypeSpec :: [CDeclSpec] -> CTypeSpec
extractTypeSpec [] = $abort "type specifier expected"
extractTypeSpec (CTypeSpec ts:_) = ts
extractTypeSpec (_:xs) = extractTypeSpec xs

apply :: (CFunDef -> a) -> (CDecl -> a) -> Ast -> Symbol -> Maybe a
apply fdf cdf ast name =
  case function_definition ast name of
    Just fd -> Just $ fdf fd
    Nothing -> case function_declaration ast name of
      Just cd -> Just $ cdf cd
      Nothing -> Nothing

addDecls :: SymbolTable -> CDecl -> SymbolTable
addDecls st cd = Map.insert (symbol cd) cd st
