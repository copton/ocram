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
import Ocram.Util (abort, unexp, (?:))
import Ocram.Names (ctrLbl)
import Prelude hiding (init)

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
  ids <- nextIds 2
  let
    [(lblStart, gotoStart), (lblEnd, gotoEnd)] = map (labelGotoPair ni) ids
    body = replaceBreakContinue lblStart lblEnd $ extractBody block
    cond' = CUnary CNegOp cond ni
    if_ = CIf cond' gotoEnd Nothing ni
    body' = map CBlockStmt [lblStart, if_] ++ body ++ map CBlockStmt [gotoStart, lblEnd]
  body'' <- mapM tItem body'
  return $ CCompound [] body'' ni

tStmt (CWhile cond block True ni) = do -- do loop {{{3
  ids <- nextIds 2
  let
    [(lblStart, gotoStart), (lblEnd, _)] = map (labelGotoPair ni) ids
    body = replaceBreakContinue lblStart lblEnd $ extractBody block
    if_ = CIf cond gotoStart Nothing ni
    body' = CBlockStmt lblStart : body ++ map CBlockStmt [if_, lblEnd]
  body'' <- mapM tItem body'
  return $ CCompound [] body'' ni

tStmt (CFor init cond incr block ni) = do -- for loop {{{3
  ids <- nextIds 2
  let
    [(lblStart, gotoStart), (lblEnd, gotoEnd)] = map (labelGotoPair ni) ids 
    body = replaceBreakContinue lblStart lblEnd $ extractBody block
    exprStmt expr = CExpr (Just expr) ni
    init' = case init of
      Left Nothing -> Nothing
      Left (Just e) -> Just $ CBlockStmt $ exprStmt e
      Right d -> $abort $ unexp d
    incr' = fmap exprStmt incr
    if_ = fmap (\c -> CIf (CUnary CNegOp c ni) gotoEnd Nothing ni) cond
    body' =
         init' ?: 
         map CBlockStmt (lblStart : if_ ?: []) 
      ++ body
      ++ map CBlockStmt (incr' ?: gotoStart : lblEnd : [])
  body'' <- mapM tItem body'
  return $ CCompound [] body'' ni

-- if statements {{{3
tStmt o@(CIf _ (CGoto _ _) Nothing _) = return o
tStmt o@(CIf _ (CGoto _ _) (Just (CGoto _ _)) _) = return o

tStmt (CIf cond then_ Nothing ni) = do
  ids <- nextIds 2
  let
    [(lblThen, gotoThen), (lblEnd, gotoEnd)] = map (labelGotoPair ni) ids 
    if_ = CIf cond gotoThen (Just gotoEnd) ni
    then_' = CCompound [] (CBlockStmt lblThen : extractBody then_) ni
    body = map CBlockStmt [if_, then_', lblEnd]
  body' <- mapM tItem body
  return $ CCompound [] body' ni

tStmt (CIf cond then_ (Just else_) ni) = do
  ids <- nextIds 3
  let
    [(lblThen, gotoThen), (lblElse, gotoElse), (lblEnd, gotoEnd)] = map (labelGotoPair ni) ids 
    if_ = CIf cond gotoThen (Just gotoElse) ni
    then_' = CCompound [] (CBlockStmt lblThen : extractBody then_ ++ [CBlockStmt gotoEnd]) ni
    else_' = CCompound [] (CBlockStmt lblElse : extractBody else_) ni
    body = map CBlockStmt [if_, then_', else_', lblEnd]
  body' <- mapM tItem body
  return $ CCompound [] body' ni

tStmt x = return x

extractBody :: CStat -> [CBlockItem]  -- {{{2
extractBody (CCompound _ body _) = body
extractBody o                    = [CBlockStmt o]

labelGotoPair :: NodeInfo -> Int -> (CStat, CStat) -- {{{2
labelGotoPair ni lid =
  let
    identifier = (internalIdent . ctrLbl) lid
    label = CLabel identifier (CExpr Nothing ni) [] ni
    goto = CGoto identifier ni
  in
    (label, goto)

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

nextIds :: Int -> S [Int] -- {{{2
nextIds count = do
  idx <- get
  modify (+count)
  return $ map (idx+) [0..(count-1)]

type S = State Int
