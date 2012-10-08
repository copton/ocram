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
import Language.C.Data.Node (NodeInfo, undefNode)
import Language.C.Syntax.AST
import Ocram.Util (abort, unexp, (?:))
import Ocram.Names (ctrLbl)
import Prelude hiding (init)

desugar_control_structures :: [CBlockItem] -> [CStat] -- {{{1
desugar_control_structures items = concat $ evalState (mapM tItem items) 0

tItem :: CBlockItem -> S [CStat] -- {{{2
tItem (CBlockStmt s) = tStmt s
tItem x              = $abort $ unexp x

tItems :: [CBlockItem] -> S [CStat] -- {{{2
tItems items = mapM tItem items >>= return . concat

tStmt :: CStat -> S [CStat] -- {{{2
tStmt (CCompound _ items _) = tItems items

tStmt (CWhile cond block False ni) = do -- while loop {{{3
  ids <- nextIds 2
  body          <- tItems $ extractBody block
  let
    [start, end] = map (labelGotoPair ni) ids
    body'        = replaceBreakContinue start end body
    cond'        = CUnary CNegOp cond ni
    if_          = CIf cond' (goto end) Nothing ni
  return         $ (label start : if_ : []) ++ body' ++ (goto start : label end : [])

tStmt (CWhile cond block True ni) = do -- do loop {{{3
  ids <- nextIds 2
  body          <- tItems $ extractBody block
  let
    [start, end] = map (labelGotoPair ni) ids
    body'        = replaceBreakContinue start end body
    if_          = CIf cond (goto start) Nothing ni
  return         $ (label start : []) ++ body' ++ (if_ : label end : [])

tStmt (CFor init cond incr block ni) = do -- for loop {{{3
  ids <- nextIds 3
  body           <- tItems $ extractBody block
  let
    [start, cont, end] = map (labelGotoPair ni) ids 
    body'              = replaceBreakContinue cont end body
    exprStmt expr      = CExpr (Just expr) ni
    init'              = case init of
                          Left Nothing -> Nothing
                          Left (Just e) -> Just $ exprStmt e
                          Right d -> $abort $ unexp d
    incr'         = fmap exprStmt incr
    if_           = fmap (\c -> CIf (CUnary CNegOp c ni) (goto end) Nothing ni) cond
  return          $ (init' ?: label start : if_ ?: []) ++ body' ++ (label cont : incr' ?: goto start : label end : [])

-- if statements {{{3
tStmt o@(CIf _ (CGoto _ _) Nothing _)            = return [o]
tStmt o@(CIf _ (CGoto _ _) (Just (CGoto _ _)) _) = return [o]

tStmt (CIf cond thenBlock Nothing ni) = do
  ids <- nextIds 2
  thenBlock'     <- tItems $ extractBody thenBlock
  let
    [thenLG, end] = map (labelGotoPair ni) ids 
    if_           = CIf cond (goto thenLG) (Just (goto end)) ni
  return          $ [if_, label thenLG] ++ thenBlock' ++ [label end]

tStmt (CIf cond thenBlock (Just elseBlock) ni) = do
  ids <- nextIds 3
  thenBlock'             <- tItems $ extractBody thenBlock
  elseBlock'             <- tItems $ extractBody elseBlock
  let
    [thenLG, elseLG, end] = map (labelGotoPair ni) ids 
    if_                   = CIf cond (goto thenLG) (Just (goto elseLG)) ni
  return                  $ (if_ : label thenLG : []) ++ thenBlock' ++ (goto end : label elseLG : []) ++ elseBlock' ++ (label end : [])

-- labels {{{3
tStmt o@(CLabel _ (CExpr Nothing _) _ _) = return [o]
tStmt (CLabel x1 stmt x2 x3) = return [CLabel x1 (CExpr Nothing undefNode) x2 x3, stmt]

tStmt (CSwitch switchExpr (CCompound _ items _) ni) =  -- {{{3
  -- the possible structures of switch statements is limited. See Analysis.Filter 
  let
    cases = groupCases items
  in do
    ids <- nextIds $ length cases + 1
    let
      (end:lgs) = map (labelGotoPair ni) ids
      ifs       = map (uncurry buildIf) $ zip (map fst cases) (map goto lgs)
      dflt      = case reverse cases of
          ((Nothing, _):_) -> Nothing
          _                -> Just $ goto end
    blocks     <- mapM (buildBlock end) $ zip (map snd cases) lgs
    return $ ifs ++ (dflt ?: []) ++ concat blocks ++ (label end : [])
  where
    buildIf (Just cmpExpr) gto = CIf (condition cmpExpr) gto Nothing undefNode
    buildIf Nothing        gto = gto
    condition rhs = CBinary CEqOp switchExpr rhs undefNode
    buildBlock end (is, lg) = tItems is >>= return . (label lg :) . replaceBreakContinue undefined end
      -- switch statements do not contain continue statements
    
tStmt x = return [x] -- {{{3

groupCases :: [CBlockItem] -> [(Maybe CExpr, [CBlockItem])] -- {{{2
groupCases xs = case xs of
  [] -> []
  (CBlockStmt item):rest -> case item of
    CCase expr stmt _ ->
      let stmt' = CBlockStmt stmt in
      if isCaseDefault stmt'
        then (Just expr, [CBlockStmt (CExpr Nothing undefNode)]) : groupCases (stmt':rest)
        else 
          let (block, others) = span (not . isCaseDefault) rest in
          (Just expr, CBlockStmt stmt : block) : groupCases others
    CDefault stmt _   ->
      let (block, others) = span (not . isCaseDefault) rest in
      (Nothing, CBlockStmt stmt : block) : groupCases others
    x -> $abort $ unexp x
  (x:_) -> $abort $ unexp x
  where
    isCaseDefault (CBlockStmt (CCase _ _ _))  = True
    isCaseDefault (CBlockStmt (CDefault _ _)) = True
    isCaseDefault _                           = False

extractBody :: CStat -> [CBlockItem]  -- {{{2
extractBody (CCompound _ body _) = body
extractBody o                    = [CBlockStmt o]

data LblGotoPair = LblGotoPair {
    label :: CStat
  , goto  :: CStat
  }

labelGotoPair :: NodeInfo -> Int -> LblGotoPair -- {{{2
labelGotoPair ni lid =
  let
    identifier = (internalIdent . ctrLbl) lid
    lbl = CLabel identifier (CExpr Nothing ni) [] ni
    gto = CGoto identifier undefNode
  in
    LblGotoPair lbl gto

replaceBreakContinue :: LblGotoPair -> LblGotoPair -> [CStat] -> [CStat] -- {{{2
replaceBreakContinue start end = everywhereBut (mkQ False blocks) (mkT trans)
  where
    blocks :: CStat -> Bool
    blocks (CSwitch _ _ _)  = True
    blocks (CWhile _ _ _ _) = True
    blocks (CFor _ _ _ _ _) = True
    blocks _                = False

    trans :: CStat -> CStat
    trans (CBreak ni) = amap (const ni) (goto end)
    trans (CCont ni)  = amap (const ni) (goto start)
    trans o = o

nextIds :: Int -> S [Int] -- {{{2
nextIds count = do
  idx <- get
  modify (+count)
  return $ map (idx+) [0..(count-1)]

type S = State Int
