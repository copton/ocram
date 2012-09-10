{-# LANGUAGE TemplateHaskell #-}
module Ocram.Intermediate.DesugarControlStructures
-- exports {{{1
(
  desugar_control_structures
) where

-- imports {{{1
import Control.Monad.State (State, evalState, get, modify)
import Data.Generics (everywhereBut, mkQ, mkT)
import Language.C.Data.Ident (internalIdent)
import Language.C.Data.Node (NodeInfo)
import Language.C.Syntax.AST
import Ocram.Util (abort, unexp, tmap)
import Ocram.Names (ctrLbl)

desugar_control_structures :: [CBlockItem] -> [CBlockItem] -- {{{1
desugar_control_structures items = evalState (mapM tItem items) 0

tItem :: CBlockItem -> S CBlockItem -- {{{2
tItem (CBlockStmt s) = fmap CBlockStmt $ tStmt s
tItem x              = $abort $ unexp x

tStmt :: CStat -> S CStat -- {{{2
tStmt (CCompound x1 items x2) = do
  items' <- mapM tItem items
  return $ CCompound x1 items' x2

tStmt (CWhile cond block False ni) = do -- while loop {{{3
  ids <- nextIds
  let
    ((lblStart, lblEnd), (gotoStart, gotoEnd)) = createLabels ids ni
    body = replaceBreakContinue lblStart lblEnd $ extractBody block
    cond' = CUnary CNegOp cond ni
    if_ = CIf cond' gotoEnd Nothing ni
    body' = map CBlockStmt [lblStart, if_] ++ body ++ map CBlockStmt [gotoStart, lblEnd]
  return $ CCompound [] body' ni

tStmt (CWhile cond block True ni) = do -- do loop {{{3
  ids <- nextIds
  let
    ((lblStart, lblEnd), (gotoStart, _)) = createLabels ids ni
    body = replaceBreakContinue lblStart lblEnd $ extractBody block
    if_ = CIf cond gotoStart Nothing ni
    body' = CBlockStmt lblStart : body ++ map CBlockStmt [if_, lblEnd]
  return $ CCompound [] body' ni

-- tStmt o@(CFor _ _ _ _ _) = do
--   ids <- nextIds
--   return $ desugarFor ids o

tStmt x = return x

extractBody :: CStat -> [CBlockItem]  -- {{{2
extractBody (CCompound _ body _) = body
extractBody o                    = [CBlockStmt o]

createLabels :: (Int, Int) -> NodeInfo -> ((CStat, CStat), (CStat, CStat)) -- {{{2
createLabels ids ni =
  let
    identifiers = tmap (internalIdent . ctrLbl) ids
    labels = tmap (\i -> CLabel i (CExpr Nothing ni) [] ni) identifiers
    gotos = tmap (\i -> CGoto i ni) identifiers
  in 
    (labels, gotos)

replaceBreakContinue :: CStat -> CStat -> [CBlockItem] -> [CBlockItem] -- {{{2
replaceBreakContinue lblStart lblEnd = everywhereBut (mkQ False blocks) (mkT trans)
  where
    blocks :: CStat -> Bool
    blocks (CSwitch _ _ _) = True
    blocks (CWhile _ _ _ _) = True
    blocks (CFor _ _ _ _ _) = True
    blocks _ = False

    trans :: CStat -> CStat
    trans (CBreak ni) = CGoto (lblIdent lblEnd) ni
    trans (CCont ni) = CGoto (lblIdent lblStart) ni
    trans o = o
    lblIdent (CLabel i _ _ _) = i
    lblIdent o = $abort $ unexp o

nextIds :: S (Int, Int) -- {{{2
nextIds = do
  idx <- get
  modify (+2)
  return $ (idx, idx+1)

type S = State Int
