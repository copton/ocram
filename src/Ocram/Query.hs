{-# LANGUAGE TemplateHaskell #-}
module Ocram.Query
-- export {{{1
(
    function_definition, function_declaration, function_info
  , is_blocking_function, is_start_function
  , is_blocking_function', is_start_function'
  , FunctionInfo(..), SymbolTable
  , function_parameters, function_parameters'
) where

-- import {{{1
import Data.Maybe (catMaybes, isJust)
import Ocram.Symbols (Symbol, symbol)
import Ocram.Names (blockingAttr, startAttr)
import Ocram.Types (Ast)
import Ocram.Util (abort, fromJust_s)
import Ocram.Visitor (traverseCFunDef, DownVisitor(..), UpVisitor(..), ListVisitor(..))
import Data.Monoid (Monoid, mappend, mempty)
import Language.C.Syntax.AST
import Language.C.Data.Ident (Ident(Ident))
import qualified Data.List as List
import qualified Data.Map as Map

data FunctionInfo = FunctionInfo { -- {{{1
      fiResultType :: CTypeSpec
    , fiParams :: [CDecl]
    , fiVariables :: SymbolTable
    , fiBody :: Maybe CStat
  } deriving (Show)

type SymbolTable = Map.Map Symbol CDecl

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

function_parameters :: CFunDef -> [CDecl] -- {{{1
function_parameters (CFunDef _ (CDeclr _ [cfd] _ _ _) [] _ _) = function_parameters' cfd
function_parameters _ = $abort "unexpected parameters" 

function_parameters' :: CDerivedDeclr -> [CDecl] -- {{{1
function_parameters' (CFunDeclr (Right (ps, False)) _ _) = ps
function_parameters' _ = $abort "unexpected parameters"

function_info :: Ast -> Symbol -> Maybe FunctionInfo -- {{{1
function_info ast name = 
  let
    fdef = function_definition ast name
    fdecl = function_declaration ast name
  in
    if isJust fdef then fmap def2fi fdef else fmap decl2fi fdecl

decl2fi :: CDecl -> FunctionInfo -- {{{2
decl2fi (CDecl tss [(Just (CDeclr _ [cfd] _ _ _), Nothing, Nothing)] _) =
  FunctionInfo (extractTypeSpec tss) params (params2SymTab params) Nothing
  where
    params = function_parameters' cfd
decl2fi _ = $abort "unexpected parameters"

def2fi :: CFunDef -> FunctionInfo -- {{{2
def2fi fd = $fromJust_s $ uFi $ snd $ traverseCFunDef fd $ DownState Map.empty []

data DownState = DownState {
    dSt :: SymbolTable
  , dPs :: [CDecl]
  }

data UpState = UpState {
    uFi :: Maybe FunctionInfo
  , uSt :: SymbolTable
  }

instance Monoid UpState where
  mempty = UpState Nothing mempty
  mappend (UpState a b) (UpState a' b') = UpState (merge a a') (mappend b b')
    where
      merge Nothing Nothing = Nothing
      merge (Just x) Nothing = Just x
      merge Nothing (Just x) = Just x
      merge (Just _) (Just _) = $abort "got two distinct function infos from one function"

instance DownVisitor DownState where
  -- add parameters to symbol table
  downCFunDef fd _ = DownState (params2SymTab params) params
    where
      params = function_parameters fd

  -- add declarations to symbol table
  downCBlockItem (CBlockDecl cd) d = d {dSt = Map.insert (symbol cd) cd (dSt d)}

  downCBlockItem _ d = d

instance UpVisitor DownState UpState where
  -- pass declarations from down state to up state
  upCBlockItem o@(CBlockDecl _) d u = (o, u `mappend` UpState Nothing (dSt d))
  upCBlockItem o _ u = (o, u)

  -- create function info entry
  upCFunDef o@(CFunDef tss _ _ body _) d u = (o, UpState (Just fi) mempty)
    where
      fi = FunctionInfo (extractTypeSpec tss) (dPs d) (dSt d `mappend` uSt u) (Just body)

instance ListVisitor DownState UpState where
  -- remove variable declarations
  nextCBlockItem (CBlockDecl _) d u = ([], d, u)
  nextCBlockItem o d u = ([o], d, u)


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


params2SymTab :: [CDecl] -> SymbolTable
params2SymTab params = foldl add Map.empty params
  where
    add m cd = Map.insert (symbol cd) cd m

extractTypeSpec :: [CDeclSpec] -> CTypeSpec
extractTypeSpec [] = $abort "type specifier expected"
extractTypeSpec ((CTypeSpec ts):_) = ts
extractTypeSpec (_:xs) = extractTypeSpec xs
