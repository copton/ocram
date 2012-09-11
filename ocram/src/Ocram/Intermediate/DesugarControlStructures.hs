{-# LANGUAGE TemplateHaskell #-}
module Ocram.Intermediate.DesugarControlStructures
-- exports {{{1
(
  desugar_control_structures
) where

-- imports {{{1
import Control.Monad.State (State, evalState, get, modify)
import Data.Generics (everywhereBut, mkQ, mkT)
import Data.List (sortBy)
import Language.C.Data.Ident (internalIdent)
import Language.C.Data.Node (NodeInfo, undefNode)
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
    [start, end] = map (labelGotoPair ni) ids
    body         = replaceBreakContinue start end $ extractBody block
    cond'        = CUnary CNegOp cond ni
    if_          = CIf cond' (goto end) Nothing ni
    body'        = map CBlockStmt [label start, if_] ++ body ++ map CBlockStmt [goto start, label end]
  body'' <- mapM tItem body'
  return $ CCompound [] body'' ni

tStmt (CWhile cond block True ni) = do -- do loop {{{3
  ids <- nextIds 2
  let
    [start, end] = map (labelGotoPair ni) ids
    body         = replaceBreakContinue start end $ extractBody block
    if_          = CIf cond (goto start) Nothing ni
    body'        = CBlockStmt (label start) : body ++ map CBlockStmt [if_, label end]
  body'' <- mapM tItem body'
  return $ CCompound [] body'' ni

tStmt (CFor init cond incr block ni) = do -- for loop {{{3
  ids <- nextIds 2
  let
    [start, end]  = map (labelGotoPair ni) ids 
    body          = replaceBreakContinue start end $ extractBody block
    exprStmt expr = CExpr (Just expr) ni
    init'         = case init of
                      Left Nothing -> Nothing
                      Left (Just e) -> Just $ CBlockStmt $ exprStmt e
                      Right d -> $abort $ unexp d
    incr'         = fmap exprStmt incr
    if_           = fmap (\c -> CIf (CUnary CNegOp c ni) (goto end) Nothing ni) cond
    body'         = init' ?: 
                       map CBlockStmt (label start : if_ ?: []) 
                    ++ body
                    ++ map CBlockStmt (incr' ?: goto start : label end : [])
  body'' <- mapM tItem body'
  return $ CCompound [] body'' ni

-- if statements {{{3
tStmt o@(CIf _ (CGoto _ _) Nothing _) = return o
tStmt o@(CIf _ (CGoto _ _) (Just (CGoto _ _)) _) = return o

tStmt (CIf cond thenBlock Nothing ni) = do
  ids <- nextIds 2
  let
    [thenLG, end] = map (labelGotoPair ni) ids 
    if_           = CIf cond (goto thenLG) (Just (goto end)) ni
    thenBlock'    = CCompound [] (CBlockStmt (label thenLG) : extractBody thenBlock) ni
    body          = map CBlockStmt [if_, thenBlock', label end]
  body' <- mapM tItem body
  return $ CCompound [] body' ni

tStmt (CIf cond thenBlock (Just elseBlock) ni) = do
  ids <- nextIds 3
  let
    [thenLG, elseLG, end] = map (labelGotoPair ni) ids 
    if_                   = CIf cond (goto thenLG) (Just (goto elseLG)) ni
    thenBlock'            = CCompound [] (CBlockStmt (label thenLG) : extractBody thenBlock ++ [CBlockStmt (goto end)]) ni
    elseBlock'            = CCompound [] (CBlockStmt (label elseLG) : extractBody elseBlock) ni
    body                  = map CBlockStmt [if_, thenBlock', elseBlock', label end]
  body' <- mapM tItem body
  return $ CCompound [] body' ni

tStmt (CSwitch switchExpr (CCompound _ items _) ni) = 
  let
    cases = groupCases items
  in do
    ids <- nextIds $ length cases + 2
    let
      (end:lgs) = map (labelGotoPair ni) ids
      ifs       = map (uncurry buildIf) $ zip (map fst cases) (map goto lgs)
      blocks    = map (uncurry (buildBlock end)) $ zip (map snd cases) lgs
      body      = map CBlockStmt $ ifs ++ blocks ++ [label end]
    body' <- mapM tItem body
    return $ CCompound [] body' ni
  where
    buildIf (Just cmpExpr) gto = CIf (condition cmpExpr) gto Nothing undefNode
    buildIf Nothing        gto = gto
    condition rhs = CBinary CEqOp switchExpr rhs undefNode
    buildBlock end items' lg =
      let items'' = replaceBreakContinue undefined end items' in -- switch statements do not contain continue statements
      CCompound [] (CBlockStmt (label lg) : items'') undefNode
    
tStmt x = return x

groupCases :: [CBlockItem] -> [(Maybe CExpr, [CBlockItem])]
groupCases = sortBy cmp . group
  where
    group xs = case xs of
      [] -> []
      (CBlockStmt item):rest -> case item of
        CCase expr stmt _ ->
          let (block, others) = span (not . isCaseDefault) rest in
          (Just expr, CBlockStmt stmt : block) : group others
        CDefault stmt _   ->
          let (block, others) = span (not . isCaseDefault) rest in
          (Nothing, CBlockStmt stmt : block) : group others
        x -> $abort $ unexp x
      (x:_) -> $abort $ unexp x

    cmp (Nothing, _) (Just _ , _) = GT
    cmp (Just _ , _) (Nothing, _) = LT
    cmp _            _            = EQ
  
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

replaceBreakContinue :: LblGotoPair -> LblGotoPair -> [CBlockItem] -> [CBlockItem] -- {{{2
replaceBreakContinue start end = everywhereBut (mkQ False blocks) (mkT trans)
  where
    blocks :: CStat -> Bool
    blocks (CSwitch _ _ _)  = True
    blocks (CWhile _ _ _ _) = True
    blocks (CFor _ _ _ _ _) = True
    blocks _                = False

    trans :: CStat -> CStat
    trans (CBreak ni) = (amap (const ni) . goto) end
    trans (CCont ni)  = (amap (const ni) . goto) start
    trans o = o

nextIds :: Int -> S [Int] -- {{{2
nextIds count = do
  idx <- get
  modify (+count)
  return $ map (idx+) [0..(count-1)]

type S = State Int
