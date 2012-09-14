{-# LANGUAGE TemplateHaskell, GADTs #-}
module Ocram.Intermediate.CriticalVariables
-- exports {{{1
(
  critical_variables
) where

-- imports {{{1
import Control.Applicative ((<$>), (<*>))
import Data.Generics (everything, mkQ)
import Data.Maybe (fromMaybe)
import Compiler.Hoopl hiding ((<*>), Label)
import Language.C.Syntax.AST
import Ocram.Intermediate.Representation
import Ocram.Util (abort)
import Ocram.Symbols (Symbol, symbol)

import qualified Data.Set as S

critical_variables :: Function -> Function -- {{{1
critical_variables (Function cvars [] fd body entry) =
  let
    facts = runLiveness entry body
    criticalLabels = S.fromList $ foldGraphNodes criticalLabel body [] 
    criticalFacts = filter ((`S.member` criticalLabels) . fst) (mapToList facts)
    criticalVars = S.unions (map snd criticalFacts)
    nonCriticalVars = [x | x <- cvars, not (S.member (var_fqn x) criticalVars)]
    truelyCriticalVars = [x | x <- cvars, S.member (var_fqn x) criticalVars]
  in
    Function truelyCriticalVars nonCriticalVars fd body entry
    
  where
    criticalLabel (Call _ l) ls = hLabel l : ls
    criticalLabel _          ls = ls


critical_variables _ = $abort "invalid function parameter"

-- types {{{1
type Var = String -- {{{2

type LiveFact = S.Set Var -- {{{2

liveLattice :: DataflowLattice LiveFact -- {{{1
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

liveness :: BwdTransfer Node LiveFact -- {{{1
liveness = mkBTransfer3 firstLive middleLive lastLive
  where
    firstLive :: Node C O -> LiveFact -> LiveFact
    firstLive (Label _)   = id

    middleLive :: Node O O -> LiveFact -> LiveFact
    middleLive (Stmt expr) = addUses expr . delUses expr

    lastLive :: Node O C -> FactBase LiveFact -> LiveFact
    lastLive (Goto l)     f =
      fact l f

    lastLive (If e tl el) f =
      addUses e . (S.union <$> fact tl <*> fact el) $ f

    lastLive (Call (FirstNormalForm _ ps) l) f =
      foldr addUses (fact l f) ps

    lastLive (Call (SecondNormalForm lhs CAssignOp _  ps) l) f =
      flip (foldr addUses) ps $ delUses lhs $ fact l f

    lastLive (Call (SecondNormalForm lhs _ _  ps) l) f =
      addUses lhs $ flip (foldr addUses) ps $ delUses lhs $ fact l f

    lastLive (Return Nothing)   _ =
      fact_bot liveLattice

    lastLive (Return (Just e))  _ =
      addUses e (fact_bot liveLattice)

    fact :: Label -> FactBase LiveFact -> LiveFact
    fact l = fromMaybe S.empty . lookupFact (hLabel l)

    addUses :: CExpr -> LiveFact -> LiveFact
    addUses expr facts = foldr S.insert facts $ readAccess expr

    delUses :: CExpr -> LiveFact -> LiveFact
    delUses expr facts = foldr S.delete facts $ writeAccess expr

readAccess :: CExpr -> [Symbol] -- {{{1
readAccess = everything (++) (mkQ [] select) -- TODO
  where
    select :: CExpr -> [Symbol]
    select (CVar i _) = [symbol i]
    select _          = []

writeAccess :: CExpr -> [Symbol] -- {{{1
writeAccess = const [] -- TODO

runLiveness :: Label -> Graph Node C C -> FactBase LiveFact -- {{{1
runLiveness entry graph = runSimpleUniqueMonad $ runWithFuel infiniteFuel result
  where
    result :: CheckingFuelMonad SimpleUniqueMonad (FactBase LiveFact)
    result = do
      (_, facts, _) <- analyzeAndRewriteBwd live (JustC [hLabel entry]) graph mapEmpty
      return facts 

    live = BwdPass {
            bp_lattice  = liveLattice
          , bp_transfer = liveness
          , bp_rewrite  = noBwdRewrite
          }
