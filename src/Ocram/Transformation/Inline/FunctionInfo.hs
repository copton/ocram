module Ocram.Transformation.Inline.FunctionInfo
-- exports {{{1
(
  function_info
) where
-- imports {{{1
import Data.Monoid
import Language.C.Syntax.AST
import Ocram.Analysis (function_definition, function_declaration)
import Ocram.Symbols (symbol)
import Ocram.Transformation.Inline.Types
import Ocram.Types (Ast)
import Ocram.Visitor
import qualified Data.Map as Map

-- function_info :: CallGraph -> Symbol -> Maybe FunctionInfo {{{1
function_info :: CallGraph -> Symbol -> Maybe FunctionInfo
function_info cg fname
  | is_blocking cg fname = fmap decl2fi $ function_declaration cg fname
  | otherwise = fmap def2fi $ function_definition cg fname

-- decl2fi :: CDecl -> FunctionInfos {{{2
decl2fi :: CDecl -> FunctionInfo
decl2fi cd@(CDecl tss [(Just (CDeclr _ [cfd] _ _ _), Nothing, Nothing)] _) =
  FunctionInfo (extractTypeSpec tss) params (params2SymTab params) Nothing
  where
    params = extractParams' cfd

-- def2fi :: CFunDef -> FunctionInfo {{{2
def2fi :: CFunDef -> FunctionInfo
def2fi fd = uFi $ snd $ traverseCFunDef fd $ DownState Map.empty []

data DownState = DownState {
    dSt :: SymTab
  , dPs :: [CDecl]
  }

data UpState = UpState {
    uFi :: FunctionInfos
  , uSt :: SymTab
  }

instance Monoid UpState where
  mempty = UpState mempty mempty
  mappend (UpState a b) (UpState a' b') = UpState (mappend a a') (mappend b b')

instance DownVisitor DownState where
  -- add parameters to symbol table {{{2
  downCFunDef fd _ = DownState (params2SymTab params) params
    where
      params = extractParams fd

  -- add declarations to symbol table {{{2
  downCBlockItem (CBlockDecl cd) d = d {dSt = Map.insert (symbol cd) cd (dSt d)}

  downCBlockItem _ d = d

instance UpVisitor DownState UpState where
  -- pass declarations from down state to up state {{{2
  upCBlockItem o@(CBlockDecl _) d u = (o, u `mappend` UpState mempty (dSt d))
  upCBlockItem o _ u = (o, u)

  -- create function info entry {{{2
  upCFunDef o@(CFunDef tss _ _ body _) d u = (o, UpState (Map.singleton name fi) mempty)
    where
      name = symbol o
      fi = FunctionInfo (extractTypeSpec tss) (dPs d) (dSt d `mappend` uSt u) (Just body)

instance ListVisitor DownState UpState where
  -- remove variable declarations {{{2
  nextCBlockItem (CBlockDecl _) d u = ([], d, u)
  nextCBlockItem o d u = ([o], d, u)

-- utils {{{1
extractParams :: CFunDef -> [CDecl]
extractParams (CFunDef _ (CDeclr _ [cfd] _ _ _) [] _ _) = extractParams' cfd

extractParams' :: CDerivedDeclr -> [CDecl]
extractParams' (CFunDeclr (Right (ps, False)) _ _) = ps


-- utils {{{2
params2SymTab :: [CDecl] -> SymTab
params2SymTab params = foldl add Map.empty params
  where
    add m cd = Map.insert (symbol cd) cd m


extractTypeSpec :: [CDeclSpec] -> CTypeSpec
extractTypeSpec [] = error "assertion failed: type specifier expected"
extractTypeSpec ((CTypeSpec ts):xs) = ts
extractTypeSpec (_:xs) = extractTypeSpec xs
