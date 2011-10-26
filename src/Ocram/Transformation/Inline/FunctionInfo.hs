module Ocram.Transformation.Inline.FunctionInfo
-- exports {{{1
(
  function_info
) where
-- imports {{{1
import Data.Monoid
import Language.C.Syntax.AST
import Ocram.Analysis (function_definition, function_declaration, CallGraph, is_critical, is_blocking)
import Ocram.Symbols (symbol, Symbol)
import Ocram.Transformation.Inline.Types
import Ocram.Util (fromJust_s, abort)
import Ocram.Visitor
import qualified Data.Map as Map

function_info :: CallGraph -> Symbol -> Maybe FunctionInfo -- {{{1
function_info cg fname
  | is_blocking cg fname = fmap decl2fi $ function_declaration cg fname
  | is_critical cg fname = fmap def2fi $ function_definition cg fname
  | otherwise = Nothing

decl2fi :: CDecl -> FunctionInfo -- {{{2
decl2fi (CDecl tss [(Just (CDeclr _ [cfd] _ _ _), Nothing, Nothing)] _) =
  FunctionInfo (extractTypeSpec tss) params (params2SymTab params) Nothing
  where
    params = extractParams' cfd
decl2fi _ = abort "unexpected parameter to decl2fi"

def2fi :: CFunDef -> FunctionInfo -- {{{2
def2fi fd = fromJust_s "FunctionInfo/1" $ uFi $ snd $ traverseCFunDef fd $ DownState Map.empty []

data DownState = DownState {
    dSt :: SymTab
  , dPs :: [CDecl]
  }

data UpState = UpState {
    uFi :: Maybe FunctionInfo
  , uSt :: SymTab
  }

instance Monoid UpState where
  mempty = UpState Nothing mempty
  mappend (UpState a b) (UpState a' b') = UpState (merge a a') (mappend b b')
    where
      merge Nothing Nothing = Nothing
      merge (Just x) Nothing = Just x
      merge Nothing (Just x) = Just x
      merge (Just _) (Just _) = abort "got two distinct function infos from one function"

instance DownVisitor DownState where
  -- add parameters to symbol table
  downCFunDef fd _ = DownState (params2SymTab params) params
    where
      params = extractParams fd

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

-- utils {{{2
extractParams :: CFunDef -> [CDecl]
extractParams (CFunDef _ (CDeclr _ [cfd] _ _ _) [] _ _) = extractParams' cfd
extractParams _ = abort "unexpected parameter to extractParams"

extractParams' :: CDerivedDeclr -> [CDecl]
extractParams' (CFunDeclr (Right (ps, False)) _ _) = ps
extractParams' _ = abort "unexpected parameter to extractParams'"

params2SymTab :: [CDecl] -> SymTab
params2SymTab params = foldl add Map.empty params
  where
    add m cd = Map.insert (symbol cd) cd m

extractTypeSpec :: [CDeclSpec] -> CTypeSpec
extractTypeSpec [] = abort "type specifier expected"
extractTypeSpec ((CTypeSpec ts):_) = ts
extractTypeSpec (_:xs) = extractTypeSpec xs
