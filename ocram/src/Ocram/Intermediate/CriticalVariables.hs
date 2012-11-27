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
import Compiler.Hoopl hiding ((<*>), Label, Body)
import Language.C.Syntax.AST
import Language.C.Syntax.Constants (cInteger)
import Ocram.Debug.Enriched (eun, CExpr')
import Ocram.Intermediate.Representation
import Ocram.Util (abort, (?:), (?++), unexp)
import Ocram.Symbols (Symbol, symbol)

import qualified Data.Set as S

critical_variables :: Function -> Function -- {{{1
critical_variables (Function vars fd body entry) =
  let
    facts = runLiveness entry body
    criticalLabels = S.fromList $ foldGraphNodes criticalLabel body [] 
    criticalFacts = filter ((`S.member` criticalLabels) . fst) (mapToList facts)
    undecidableIdents = S.fromList $ undecidable vars body
    criticalIdents = S.unions $ undecidableIdents : map snd criticalFacts
    vars' = map setUncritical vars
    setUncritical var
      | not (S.member (symbol var) criticalIdents) = var {fvar_critical = False}
      | otherwise = var
  in Function vars' fd body entry
    
  where
    criticalLabel (Call _ l) ls = hLabel l : ls
    criticalLabel _          ls = ls

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
    firstLive (Label _)                                = id
    firstLive (Cont _ (FirstNormalForm _ _ _))         = id
    firstLive (Cont _ (SecondNormalForm lhs op _ _ _)) = addUses expr . delUses expr
      where expr = CAssign op lhs (CConst (CIntConst (cInteger 0) eun)) eun
      

    middleLive :: Node O O -> LiveFact -> LiveFact
    middleLive (Stmt expr) = addUses expr . delUses expr

    lastLive :: Node O C -> FactBase LiveFact -> LiveFact
    lastLive (Goto l)     f =
      fact l f

    lastLive (If e tl el _) f =
      addUses e . (S.union <$> fact tl <*> fact el) $ f

    lastLive (Call (FirstNormalForm _ ps _) l) f =
      foldr addUses (fact l f) ps

    lastLive (Call (SecondNormalForm _ _ _  ps _) l) f =
      foldr addUses (fact l f) ps

    lastLive (Return Nothing _)  _ =
      fact_bot liveLattice

    lastLive (Return (Just e) _) _ =
      addUses e (fact_bot liveLattice)

    fact :: Label -> FactBase LiveFact -> LiveFact
    fact l = fromMaybe S.empty . lookupFact (hLabel l)

    addUses :: CExpr' -> LiveFact -> LiveFact
    addUses expr facts = foldr S.insert facts $ readAccess expr

    delUses :: CExpr' -> LiveFact -> LiveFact
    delUses expr facts = foldr S.delete facts $ writeAccess expr

readAccess :: CExpr' -> [Symbol] -- {{{1
readAccess (CComma es _)                 = concatMap readAccess es
readAccess (CAssign CAssignOp _   rhs _) = readAccess rhs
readAccess (CAssign _         lhs rhs _) = concatMap readAccess [lhs, rhs]
readAccess (CCond cond te ee _)          = concatMap readAccess $ cond : te ?: ee : []
readAccess (CBinary _ lhs rhs _)         = concatMap readAccess [lhs, rhs]
readAccess (CCast _ e _)                 = readAccess e
readAccess (CUnary _ e _)                = readAccess e
readAccess (CSizeofExpr _ _)             = []
readAccess (CSizeofType _ _)             = []
readAccess (CAlignofExpr _ _)            = []
readAccess (CAlignofType _ _)            = []
readAccess (CComplexReal e _)            = readAccess e
readAccess (CComplexImag e _)            = readAccess e
readAccess (CIndex array index _)        = concatMap readAccess [array, index]
readAccess (CCall callee params _)       = concatMap readAccess (callee : params)
readAccess (CMember lhs _ _ _)           = readAccess lhs
readAccess (CVar ident _)                = [symbol ident]
readAccess (CConst _)                    = []
readAccess (CCompoundLit _ il _)         = everything (++) (mkQ [] readAccess) il
readAccess o@(CStatExpr _ _)             = $abort $ unexp o
readAccess (CLabAddrExpr _ _)            = []
readAccess o@(CBuiltinExpr _)            = $abort $ unexp o

writeAccess :: CExpr' -> [Symbol] -- {{{1
writeAccess (CComma es _)                    = concatMap writeAccess es
writeAccess (CAssign _ (CVar ident _) rhs _) = symbol ident : writeAccess rhs
writeAccess (CAssign _ lhs rhs _)            = concatMap writeAccess [lhs, rhs]
writeAccess (CCond cond te ee _)             = concatMap writeAccess $ cond : te ?: ee : []
writeAccess (CBinary _ lhs rhs _)            = concatMap writeAccess [lhs, rhs]
writeAccess (CCast _ e _)                    = writeAccess e
writeAccess (CUnary _ e _)                   = writeAccess e
writeAccess (CSizeofExpr _ _)                = []
writeAccess (CSizeofType _ _)                = []
writeAccess (CAlignofExpr _ _)               = []
writeAccess (CAlignofType _ _)               = []
writeAccess (CComplexReal e _)               = writeAccess e
writeAccess (CComplexImag e _)               = writeAccess e
writeAccess (CIndex array index _)           = concatMap writeAccess [array, index]
writeAccess (CCall callee params _)          = concatMap writeAccess (callee : params)
writeAccess (CMember lhs _ _ _)              = writeAccess lhs
writeAccess (CVar _ _)                       = []
writeAccess (CConst _)                       = []
writeAccess (CCompoundLit _ il _)            = everything (++) (mkQ [] writeAccess) il
writeAccess o@(CStatExpr _ _)                = $abort $ unexp o
writeAccess (CLabAddrExpr _ _)               = []
writeAccess o@(CBuiltinExpr _)               = $abort $ unexp o

undecidable :: [FunctionVariable] -> Body -> [Symbol] -- {{{1
undecidable vars body = addrof
  where
    arrays = S.fromList . map symbol . filter isArray . map (var_decl . fvar_var) $ vars
    isArray (CDecl _ [(Just (CDeclr _ dds _ _ _), _, _)] _) = any isArrayDecl dds
    isArray _                                               = False
    isArrayDecl (CArrDeclr _ _ _) = True
    isArrayDecl _                 = False

    addrof :: [Symbol]
    addrof = foldGraphNodes addrof' body []

    addrof' :: Node e x -> [Symbol] -> [Symbol]
    addrof' (Label _)                                    p = p
    addrof' (Cont _ _)                                   p = p -- covered by preceding Call
    addrof' (Stmt e)                                     p = addrof'' e ++ p
    addrof' (Goto _)                                     p = p
    addrof' (If e _ _ _)                                 p = addrof'' e ++ p
    addrof' (Call (FirstNormalForm _ params _) _)        p = concatMap addrof'' params ++ p
    addrof' (Call (SecondNormalForm lhs _ _ params _) _) p = concatMap addrof'' (lhs : params) ++ p
    addrof' (Return e _)                                 p = fmap addrof'' e ?++ p

    addrof'' :: CExpr' -> [Symbol]
    addrof'' = everything (++) (mkQ [] pquery)

    pquery :: CExpr' -> [Symbol]
    pquery (CUnary CAdrOp expr _) = pbase expr
    pquery (CBinary op (CVar lhs _) _ _)
      | S.member (symbol lhs) arrays
          && isAddOrSub op        = [symbol lhs]
      | otherwise = []
    pquery _                      = []

    isAddOrSub CAddOp = True
    isAddOrSub CSubOp = True
    isAddOrSub _      = False

    pbase (CVar iden _)       = [symbol iden]
    pbase (CMember lhs _ _ _) = pbase lhs
    pbase (CIndex lhs _ _)    = pbase lhs
    pbase _                   = []


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
