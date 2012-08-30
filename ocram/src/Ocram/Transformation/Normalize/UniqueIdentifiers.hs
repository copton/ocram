{-# LANGUAGE Rank2Types, TemplateHaskell #-}
module Ocram.Transformation.Normalize.UniqueIdentifiers
-- export {{{1
(
  unique_identifiers
) where

-- import {{{1
import Control.Monad.State (runState, State, get, put, gets, modify)
import Data.Generics (gmapM, mkQ, mkM, extM, extQ, GenericQ, GenericM, Data)
import Data.Maybe (fromMaybe)
import Language.C.Syntax.AST
import Ocram.Debug (ENodeInfo(..), Substitution(..))
import Ocram.Symbols (symbol, Symbol)
import Ocram.Transformation.Util (ident)
import Ocram.Transformation.Names (varShadow)
import Ocram.Transformation.Types
import Ocram.Util (abort, unexp)
import qualified Data.Map as Map

unique_identifiers :: CFunDef' -> CFunDef' -- {{{1
unique_identifiers fd = (fst . transform initialState) fd
  where
--    globalIds = Identifiers Map.empty (foldl (\m x -> Map.insert x x m) Map.empty $ map symbol ds)
    initialState = MyState noIds (symbol fd)
    noIds = Identifiers Map.empty Map.empty

    traverse :: Monad m => GenericQ Bool -> GenericM m -> GenericM m
    traverse q f x
      | q x = f x
      | otherwise = f x >>= gmapM (traverse q f)

    transform :: Data a => MyState -> a -> (a, MyState)
    transform ids x = runState (traverse (mkQ False quitStat `extQ` quitExpr) (mkM trDecl `extM` trStat `extM` trExpr) x) ids

    quitStat :: CStat' -> Bool
    quitStat (CCompound _ _ _) = True
    quitStat _                 = False

    quitExpr :: CExpr' -> Bool
    quitExpr (CCast _ _ _)      = True
    quitExpr (CSizeofType _ _)  = True
    quitExpr (CAlignofType _ _) = True
    quitExpr _                  = False

    trDecl :: CDecl' -> State MyState CDecl'
    trDecl = renameDecl
    
    trStat :: CStat' -> State MyState CStat'
    trStat (CCompound x1 items x2) = do
      state <- get
      let (items', (MyState ids' _)) = transform state items
      modify (\(MyState ids fname) -> MyState (ids {idEs = idEs ids'}) fname)
      return $ CCompound x1 items' x2
    
    trStat o = return o

    trExpr :: CExpr' -> State MyState CExpr'
    trExpr (CVar identifier ni) = do
      ids <- gets stateIdentifiers
      let identifier' = getIdentifier ids (symbol identifier)
      return $ CVar (ident identifier') ni

    trExpr o = return o
  
type ExtraSymbols = Map.Map Symbol Int -- {{{2

type RenameTable = Map.Map Symbol Symbol -- {{{2

data Identifiers = Identifiers { -- {{{2
  idEs :: ExtraSymbols,
  idRt :: RenameTable
  } deriving (Show)

data MyState = MyState { -- {{{2
    stateIdentifiers :: Identifiers
  , stateFname       :: Symbol
  }

emptyIds :: Identifiers -- {{{2
emptyIds = Identifiers Map.empty Map.empty

getIdentifier :: Identifiers -> Symbol -> Symbol -- {{{2
getIdentifier (Identifiers _ rt) name = fromMaybe name (Map.lookup name rt)

renameDecl :: CDecl' -> State MyState CDecl' -- {{{2
renameDecl (CDecl x1 [(Just dr, y1, y2)] ni) = do
  (MyState ids fname) <- get
  let ids' = newIdentifier ids (symbol dr)
  let dr' = renameDeclr ids' dr
  let ni' = ni {enSubst = [Substitution (symbol dr) (symbol dr') fname]}
  put (MyState ids' fname)
  return $ CDecl x1 [(Just dr', y1, y2)] ni'

renameDecl o = return o
  
renameDeclr :: Identifiers -> CDeclr' -> CDeclr' -- {{{2
renameDeclr ids (CDeclr (Just oldIdent) x2 x3 x4 x5) =
  let newIdent = ident $ getIdentifier ids (symbol oldIdent) in
  CDeclr (Just newIdent) x2 x3 x4 x5
renameDeclr _ x = $abort $ unexp x

newIdentifier :: Identifiers -> Symbol -> Identifiers -- {{{2
newIdentifier (Identifiers es rt) identifier = case Map.lookup identifier rt of
  Nothing -> Identifiers es (Map.insert identifier identifier rt)
  Just _ ->
    let (es', identifier') = newExtraSymbol es identifier in
    Identifiers es' (Map.insert identifier identifier' rt)

newExtraSymbol :: ExtraSymbols -> Symbol -> (ExtraSymbols, Symbol) -- {{{2
newExtraSymbol es name = case Map.lookup name es of
  Nothing -> (Map.insert name 0 es, varShadow name 0)
  Just x -> (Map.adjust (+1) name es, varShadow name (x+1))
