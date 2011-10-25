module Ocram.Analysis.FunctionInfo
-- exports {{{1
(
  decl2fi, def2fi
) where
-- imports {{{1
import Data.Monoid
import Language.C.Syntax.AST
import Ocram.Analysis.Types (FunctionInfo(FunctionInfo), SymTab)
import Ocram.Symbols (symbol, Symbol)
import Ocram.Types (Ast)
import Ocram.Util (fromJust_s, abort)
import Ocram.Visitor
import qualified Data.Map as Map

decl2fi :: CDecl -> FunctionInfo -- {{{2
decl2fi cd@(CDecl tss [(Just (CDeclr _ [cfd] _ _ _), Nothing, Nothing)] _) =
  FunctionInfo (extractTypeSpec tss) params (params2SymTab params) Nothing
  where
    params = extractParams' cfd

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
      name = symbol o
      fi = FunctionInfo (extractTypeSpec tss) (dPs d) (dSt d `mappend` uSt u) (Just body)

instance ListVisitor DownState UpState where
  -- remove variable declarations
  nextCBlockItem (CBlockDecl _) d u = ([], d, u)
  nextCBlockItem o d u = ([o], d, u)

-- utils {{{2
extractParams :: CFunDef -> [CDecl]
extractParams (CFunDef _ (CDeclr _ [cfd] _ _ _) [] _ _) = extractParams' cfd

extractParams' :: CDerivedDeclr -> [CDecl]
extractParams' (CFunDeclr (Right (ps, False)) _ _) = ps

params2SymTab :: [CDecl] -> SymTab
params2SymTab params = foldl add Map.empty params
  where
    add m cd = Map.insert (symbol cd) cd m

extractTypeSpec :: [CDeclSpec] -> CTypeSpec
extractTypeSpec [] = abort "type specifier expected"
extractTypeSpec ((CTypeSpec ts):xs) = ts
extractTypeSpec (_:xs) = extractTypeSpec xs
