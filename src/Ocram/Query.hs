{-# LANGUAGE TemplateHaskell #-}
module Ocram.Query
-- export {{{1
(
    function_definition, function_declaration
  , function_parameters,  return_type, local_variables
  , function_parameters_fd, function_parameters_cd
  , return_type_fd, return_type_cd
  , local_variables_fd, local_variables_cd
  , is_blocking_function, is_start_function
  , is_blocking_function', is_start_function'
  , SymbolTable
  , unlist_decl
) where

-- import {{{1
import Data.Maybe (catMaybes)
import Ocram.Symbols (Symbol, symbol)
import Ocram.Names (blockingAttr, startAttr)
import Ocram.Types (Ast)
import Ocram.Util (abort)
import Ocram.Visitor (traverseCFunDef, DownVisitor(..), UpVisitor(..), ListVisitor(..))
import Language.C.Syntax.AST
import Language.C.Data.Ident (Ident(Ident))
import qualified Data.List as List
import qualified Data.Map as Map
type SymbolTable = Map.Map Symbol CDecl -- {{{1

function_declaration :: Ast -> Symbol -> Maybe CDecl -- {{{1
function_declaration (CTranslUnit eds _) name =
  List.find ((==name) . symbol) $ catMaybes $ map functionDeclaration eds

function_definition :: Ast -> Symbol -> Maybe CFunDef -- {{{1
function_definition (CTranslUnit eds _) name =
  List.find ((==name) . symbol) $ catMaybes $ map functionDefinition eds

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
local_variables_fd fd =
  let st = snd $ traverseCFunDef fd DownState in
  foldl addDecls st $ function_parameters_fd fd

data DownState = DownState
instance DownVisitor DownState
instance ListVisitor DownState SymbolTable
instance UpVisitor DownState SymbolTable where
  upCBlockItem o@(CBlockDecl cd) _ st = (o, addDecls st cd)
  upCBlockItem o _ u = (o, u)

  upCStat o@(CFor (Right cd) _ _ _ _) _ st = (o, addDecls st cd)
  upCStat o _ u = (o, u)

local_variables_cd :: CDecl -> SymbolTable -- {{{1
local_variables_cd cd = foldl addDecls Map.empty $ function_parameters_cd cd

unlist_decl :: CDecl -> [CDecl]
unlist_decl (CDecl x ds z) = map (\y -> CDecl x [y] z) ds

-- utils {{{1

isBlockingAttribute :: CDeclSpec -> Bool
isBlockingAttribute (CTypeQual (CAttrQual (CAttr (Ident attr _ _) [] _))) = attr == blockingAttr
isBlockingAttribute _ = False

isStartAttr :: CDeclSpec -> Bool
isStartAttr (CTypeQual (CAttrQual (CAttr (Ident attr _ _) [] _))) = attr == startAttr
isStartAttr _ = False

functionDeclaration :: CExtDecl -> Maybe CDecl
functionDeclaration (CDeclExt x) = Just x 
functionDeclaration _ = Nothing

functionDefinition :: CExtDecl -> Maybe CFunDef
functionDefinition (CFDefExt x) = Just x
functionDefinition _ = Nothing

functionParameters :: CDerivedDeclr -> [CDecl]
functionParameters (CFunDeclr (Right (ps, False)) _ _) = ps
functionParameters _ = $abort "unexpected parameters"

extractTypeSpec :: [CDeclSpec] -> CTypeSpec
extractTypeSpec [] = $abort "type specifier expected"
extractTypeSpec ((CTypeSpec ts):_) = ts
extractTypeSpec (_:xs) = extractTypeSpec xs

apply :: (CFunDef -> a) -> (CDecl -> a) -> Ast -> Symbol -> Maybe a
apply fdf cdf ast name =
  case function_definition ast name of
    Just fd -> Just $ fdf fd
    Nothing -> case function_declaration ast name of
      Just cd -> Just $ cdf cd
      Nothing -> Nothing

addDecls :: SymbolTable -> CDecl -> SymbolTable
addDecls st cd =
  foldl insert st $ unlist_decl cd
  where
    insert st' cd' = Map.insert (symbol cd') cd' st'
