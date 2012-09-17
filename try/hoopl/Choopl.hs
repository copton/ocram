{-# LANGUAGE GADTs #-}
module Choopl where

-- imports {{{1
import Compiler.Hoopl hiding ((<*>))
import Control.Applicative ((<$>), (<*>))
import Data.Maybe (fromMaybe, maybeToList)
import Language.C.Data.InputStream (inputStreamFromString)
import Language.C.Data.Ident (Ident(..))
import Language.C.Data.Position (nopos)
import Language.C.Syntax.AST
import Language.C.Parser (execParser_, expressionP)
import Language.C.Pretty (pretty)
import Prelude hiding (last)

import qualified Data.Set as S
import qualified Compiler.Hoopl as H

splice :: Graph Node e O -> Graph Node O x -> Graph Node e x
splice = (H.<*>)

-- intermediate representation -- {{{1

data Node e x where -- {{{2
  Label  :: Label                   -> Node C O
  Stmt   :: CExpr                   -> Node O O
  Branch :: Label                   -> Node O C
  Cond   :: CExpr -> Label -> Label -> Node O C
  Crit   :: CExpr -> Label          -> Node O C
  Return :: Maybe CExpr             -> Node O C

instance NonLocal Node where -- {{{2
  entryLabel (Label x) = x
  successors (Branch x) = [x]
  successors (Cond _ x y) = [x, y]
  successors (Crit _ x) = [x]
  successors (Return _) = []

instance Show (Node e x) where -- {{{2
  show (Label l) = show l
  show (Stmt e)  = show e
  show (Branch l) = "goto " ++ show l
  show (Cond e l1 l2) = "if " ++ show e ++ " then " ++ show l1 ++ " else " ++ show l2
  show (Crit e l)     = "call " ++ show e ++ "; goto " ++ show l
  show (Return e)     = "return " ++ show e

-- example graph -- {{{1

type M = CheckingFuelMonad (SimpleUniqueMonad) -- {{{2

example :: M (Label, Graph Node C C) -- {{{2
example = do
  lbl1 <- freshLabel
  lbl2 <- freshLabel
  lbl3 <- freshLabel

  let
    g = foldl (|*><*|) emptyClosedGraph . map toBlock $ [
        (lbl1, ["x=1", "y=4"], Cond (p "z") lbl2 lbl3)
      , (lbl2, ["x=y"], Branch lbl3)
      , (lbl3, [], Return Nothing)
      ]
  return (lbl1, g)

  where
    toBlock :: (Label, [String], Node O C) -> Graph Node C C -- {{{3
    toBlock (lbl, exprs, close) =
      let
        first   = mkFirst (Label lbl)
        middles = mkMiddles $ map (Stmt . p) exprs
        last    = mkLast close
      in first `splice` middles `splice` last
        
    p :: String -> CExpr -- {{{3
    p expr = case execParser_ expressionP (inputStreamFromString expr) nopos of
      Left e -> error (show e)
      Right x -> x

-- Liveness analysis -- {{{1
-- types {{{2
type Var = String
type LiveFact = S.Set Var

liveLattice :: DataflowLattice LiveFact -- {{{2
liveLattice = DataflowLattice {
    fact_name = "Live variables"
  , fact_bot  = S.empty
  , fact_join = add
  }
  where
    add _ (OldFact old) (NewFact new) = (c, u)
      where
        u = new `S.union` old
        c = changeIf (S.size u > S.size old)

liveness :: BwdTransfer Node LiveFact -- {{{2
liveness = mkBTransfer3 firstLive middleLive lastLive
  where
    firstLive :: Node C O -> LiveFact -> LiveFact
    firstLive (Label _)   = id

    middleLive :: Node O O -> LiveFact -> LiveFact
    middleLive (Stmt expr) = addUses expr . delUses expr

    lastLive :: Node O C -> FactBase LiveFact -> LiveFact
    lastLive (Branch l)     f = fact l f
    lastLive (Cond e tl el) f = addUses e . (S.union <$> fact tl <*> fact el) $ f
    lastLive (Crit e l)     f = addUses e . fact l $ f
    lastLive (Return e)     _ = foldr addUses (fact_bot liveLattice) (maybeToList e)

    fact :: Label -> FactBase LiveFact -> LiveFact
    fact l = fromMaybe S.empty . lookupFact l

    addUses :: CExpr -> LiveFact -> LiveFact
    addUses (CAssign _ _ v@(CVar _ _) _)        = S.insert (name v)
    addUses (CAssign _ _ (CBinary _ v1 v2 _) _) = S.insert (name v1) . S.insert (name v2)
    addUses (CAssign _ _ (CConst _) _)          = id
    addUses v@(CVar _ _)                        = S.insert (name v)
    addUses x                                   = error $ "addUses: " ++ (show . pretty) x

    delUses :: CExpr -> LiveFact -> LiveFact
    delUses (CAssign _ v _ _) = S.delete (name v)
    delUses x                 = error $ "delUses: " ++ (show . pretty) x

    name :: CExpr -> String
    name (CVar (Ident n _ _) _) = n
    name x                      = error $ "name: " ++ (show . pretty) x

runLiveness :: (Label, Graph Node C C) -> M (FactBase LiveFact) -- {{{2
runLiveness (entry, graph) = do
  (_, facts, _) <- analyzeAndRewriteBwd bwd (JustC [entry]) graph mapEmpty
  return facts 
  where
    bwd = BwdPass {
            bp_lattice  = liveLattice
          , bp_transfer = liveness
          , bp_rewrite  = noBwdRewrite
          }

-- Constant Propagation -- {{{1
type ConstFact = M.Map Var (WithTop CConst) -- {{{2

constLattice :: DataflowLattice ConstFact -- {{{2
constLattice = DataflowLattice {
    fact_name = "constant propagation"
  , fact_bot  = M.empty
  , fact_join = joinMaps (extenedJoinDomain constFactAdd)
  }
  where
    constFactAdd _ (OldFact old) (NewFact new) =
      if new == old
        then (NoChange, PElem new)
        else (SomeChange, Top)

initFact :: [Var] -> ConstFact
initFact vars = M.fromList [(v, Top) | v <- vars]

constantPropagation :: FwdRewrite

-- convert -- {{{1
-- convert :: Graph Node C C -> [Node e x]
-- convert graph = let
--   blocks = (map snd $ bodyList graph) :: Block Node e x
--   x = foldBlockNodesF (:) (head blocks) 
--   in
--     []

main :: IO ()
main = print $ runSimpleUniqueMonad $ runWithFuel fuel (example >>= analyze)
  where fuel = 9999
  
