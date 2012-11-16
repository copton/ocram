{-# LANGUAGE TemplateHaskell #-}
module Ocram.Intermediate.DesugarControlStructures
-- exports {{{1
(
  desugar_control_structures
) where

-- imports {{{1
import Control.Monad.State (State, runState, get, put)
import Data.Generics (everywhereBut, mkQ, mkT)
import Language.C.Data.Ident (internalIdent)
import Language.C.Data.Node (undefNode)
import Language.C.Syntax.AST
import Ocram.Debug.Enriched (ENodeInfo, CStat', node_start, node_end, eun, aset)
import Ocram.Intermediate.Representation
import Ocram.Symbols (symbol)
import Ocram.Util (abort, unexp, (?:))
import Ocram.Names (identDesugar)
import Prelude hiding (init)

desugar_control_structures :: [CBlockItem] -> ([CStat'], [FunctionVariable]) -- {{{1
desugar_control_structures items = 
  let
    s0 = MyState 0 []
    (stmts, MyState _ vars) = runState (mapM tItem items) s0
  in
    (concat stmts, vars)

tItem :: CBlockItem -> S [CStat'] -- {{{2
tItem (CBlockStmt s) = tStmt s
tItem x              = $abort $ unexp x

tItems :: [CBlockItem] -> S [CStat'] -- {{{2
tItems items = mapM tItem items >>= return . concat

tStmt :: CStat -> S [CStat'] -- {{{2
tStmt (CCompound _ items _) = tItems items

tStmt o@(CWhile cond block False _) = do -- while loop {{{3
  ids <- nextIds 2
  body          <- tItems $ extractBody block
  let
    [start, end] = map labelGotoPair ids
    body'        = replaceBreakContinue start end body
    cond'        = CUnary CNegOp (fmap node_start cond) (node_start cond)
    if_          = let eni = annotation cond' in
                   CIf cond' (egoto eni end) Nothing eni
    gstart       = egoto (node_end o) start
  return         $ (elabel start : if_ : []) ++ body' ++ (gstart : elabel end : [])

tStmt (CWhile cond block True _) = do -- do loop {{{3
  ids <- nextIds 2
  body          <- tItems $ extractBody block
  let
    [start, end] = map labelGotoPair ids
    body'        = replaceBreakContinue start end body
    cond'        = fmap node_start cond
    if_          = let eni = annotation cond' in
                   CIf cond' (egoto eni start) Nothing eni
  return         $ (elabel start : []) ++ body' ++ (if_ : elabel end : [])

tStmt o@(CFor init cond incr block _) = do -- for loop {{{3
  ids <- nextIds 3
  body           <- tItems $ extractBody block
  let
    [start, cont, end] = map labelGotoPair ids 
    body'              = replaceBreakContinue cont end body
    init'              = case init of
                          Left Nothing -> Nothing
                          Left (Just e) -> Just $ exprStmt e
                          Right d -> $abort $ unexp d
    incr'         = fmap exprStmt incr
    if_           = fmap (buildIf end) cond
    gstart        = egoto (node_end o) start
  return          $ (init' ?: elabel start : if_ ?: []) ++ body' ++ (elabel cont : incr' ?: gstart : elabel end : [])
  where
    buildIf :: LblGotoPair -> CExpr -> CStat'
    buildIf end cond' =
      let eni = node_start cond' in
      CIf (CUnary CNegOp (fmap node_start cond') eni) (egoto eni end) Nothing eni

    exprStmt :: CExpr -> CStat'
    exprStmt expr = CExpr (Just (fmap node_start expr)) (node_start expr)

-- if statements {{{3
tStmt o@(CIf _ (CGoto _ _) Nothing _)            = return [fmap node_start o]
tStmt o@(CIf _ (CGoto _ _) (Just (CGoto _ _)) _) = return [fmap node_start o]

tStmt o@(CIf cond thenBlock Nothing _) = do
  ids <- nextIds 2
  thenBlock'     <- tItems $ extractBody thenBlock
  let
    eni           = node_start o
    [thenLG, end] = map labelGotoPair ids 
    cond'         = fmap node_start cond
    if_           = CIf cond' (egoto eni thenLG) (Just (egoto eni end)) eni
  return          $ [if_, elabel thenLG] ++ thenBlock' ++ [elabel end]

tStmt o@(CIf cond thenBlock (Just elseBlock) _) = do
  ids <- nextIds 3
  thenBlock'             <- tItems $ extractBody thenBlock
  elseBlock'             <- tItems $ extractBody elseBlock
  let
    eni                   = node_start o
    [thenLG, elseLG, end] = map labelGotoPair ids 
    cond'                 = fmap node_start cond
    if_                   = CIf cond' (egoto eni thenLG) (Just (egoto eni elseLG)) eni
    gend                  = egoto (node_end thenBlock) end
  return                  $ (if_ : elabel thenLG : []) ++ thenBlock' ++ (gend : elabel elseLG : []) ++ elseBlock' ++ (elabel end : [])

-- labels {{{3
tStmt o@(CLabel _ (CExpr Nothing _) _ _) = return [aset eun o]
tStmt (CLabel x1 stmt x2 _) = do
  stmt' <- tStmt stmt
  return $ [CLabel x1 (CExpr Nothing eun) (map (aset eun) x2) eun] ++ stmt'

tStmt o@(CSwitch switchExpr (CCompound _ items _) _) =  -- {{{3
  -- the possible structures of switch statements is limited. See Analysis.Filter 
  let
    cases = groupCases items
  in do
    ids <- nextIds $ length cases + 1
    var <- nextVar
    let
      (end:lgs)     = map labelGotoPair ids
      ifs           = zipWith buildIf (map fst cases) lgs
      dflt          = case reverse cases of
          ((Nothing, _):_) -> Nothing
          _                -> Just $ egoto eni end
      variable      = CVar ((internalIdent . symbol) var) undefNode
      assign        = aset (node_start switchExpr) $ CExpr (Just (CAssign CAssignOp variable switchExpr undefNode)) undefNode
      condition rhs = aset eni $ CBinary CEqOp variable rhs undefNode
      buildIf (Just cmpExpr) lg = CIf (condition cmpExpr) (egoto eni lg) Nothing eni
      buildIf Nothing        lg = egoto eni lg

    blocks     <- mapM (buildBlock end) $ zip (map snd cases) lgs
    return $ assign : ifs ++ (dflt ?: []) ++ concat blocks ++ (elabel end : [])
  where
    eni                       = node_start o
    buildBlock end (is, lg)   = tItems is >>= return . (elabel lg :) . replaceBreakContinue undefined end
      -- switch statements do not contain continue statements
    
tStmt x = return [fmap node_start x] -- {{{3

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

elabel :: LblGotoPair -> CStat'
elabel = aset eun . label

egoto :: ENodeInfo -> LblGotoPair -> CStat'
egoto eni = aset eni . goto

labelGotoPair :: Int -> LblGotoPair -- {{{2
labelGotoPair lid =
  let
    identifier = (internalIdent . identDesugar) lid
    lbl = CLabel identifier (CExpr Nothing undefNode) [] undefNode
    gto = CGoto identifier undefNode
  in
    LblGotoPair lbl gto

replaceBreakContinue :: LblGotoPair -> LblGotoPair -> [CStat'] -> [CStat'] -- {{{2
replaceBreakContinue start end = everywhereBut (mkQ False blocks) (mkT trans)
  where
    blocks :: CStat' -> Bool
    blocks (CSwitch _ _ _)  = True
    blocks (CWhile _ _ _ _) = True
    blocks (CFor _ _ _ _ _) = True
    blocks _                = False

    trans :: CStat' -> CStat'
    trans (CBreak eni) = egoto eni end
    trans (CCont eni)  = egoto eni start
    trans o = o

nextIds :: Int -> S [Int] -- {{{2
nextIds count = do
  MyState idx vars <- get
  put (MyState (idx + count) vars)
  return $ map (idx+) [0..(count-1)]

nextVar :: S FunctionVariable -- {{{2
nextVar = do
  MyState idx vars <- get
  let
    declr = CDeclr (Just (internalIdent (identDesugar idx))) [] Nothing [] undefNode
    var   = fvarAuto $ EVariable $ CDecl [CTypeSpec (CIntType undefNode)] [(Just declr, Nothing, Nothing)] undefNode
  put (MyState idx (var : vars))
  return var

data MyState = MyState {
  stIdx  :: Int,
  stVars :: [FunctionVariable]
  }

type S = State MyState
