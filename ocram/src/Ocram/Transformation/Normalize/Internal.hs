module Ocram.Transformation.Normalize.Internal
-- export {{{1
(
  desugar_control_structures, explicit_return, wrap_dangling_statements, unlist_declarations
) where

-- import {{{1
import Control.Monad.State (evalState, State, get, modify)
import Data.Generics (everything, mkQ, everywhere, everywhereBut, mkT, everywhereM, mkM)
import Data.Monoid (Any(Any, getAny), mappend)
import Ocram.Analysis (is_critical)
import Ocram.Debug (ENodeInfo)
import Ocram.Query (return_type_fd)
import Ocram.Symbols (symbol, Symbol)
import Ocram.Util (abort, unexp, tmap, (?:))
import Ocram.Transformation.Names (ctrlbl)
import Ocram.Transformation.Util (un, ident)
import Language.C.Syntax.AST

desugar_control_structures :: CFunctionDef ENodeInfo -> CFunctionDef ENodeInfo -- {{{1
desugar_control_structures (CFunDef x1 x2 x3 (CCompound y1 body y2) x4) =
  CFunDef x1 x2 x3 (CCompound y1 body' y2) x4
  where
    body' = evalState (everywhereM (mkM go) body) 0

    go :: CStatement ENodeInfo -> State Int (CStatement ENodeInfo)
    go o@(CWhile _ _ _ _) = do
      ids <- nextIds
      return $ desugarWhile ids o

    go o@(CFor _ _ _ _ _) = do
      ids <- nextIds
      return $ desugarFor ids o

    go o = return o

    createLabels ids ni =
      let
        identifiers = tmap (ident . ctrlbl) ids
        labels = tmap (\i -> CLabel i (CExpr Nothing ni) [] ni) identifiers
        gotos = tmap (\i -> CGoto i ni) identifiers
      in 
        (labels, gotos)

    extractBody (CCompound _ body _) = body
    extractBody o = [CBlockStmt o]

    desugarWhile ids (CWhile cond block False ni) = -- while loop {{{2
      let
        ((lblStart, lblEnd), (gotoStart, gotoEnd)) = createLabels ids ni
        body = replaceGotoContinue lblStart lblEnd $ extractBody block
        cond' = CUnary CNegOp cond ni
        if_ = CIf cond' (wrapInBlock gotoEnd) Nothing ni
        body' = map CBlockStmt [lblStart, if_] ++ body ++ map CBlockStmt [gotoStart, lblEnd]
      in
        CCompound [] body' ni

    desugarWhile ids (CWhile cond block True ni) = -- do loop {{{2
      let
        ((lblStart, lblEnd), (gotoStart, gotoEnd)) = createLabels ids ni
        body = replaceGotoContinue lblStart lblEnd $ extractBody block
        if_ = CIf cond (wrapInBlock gotoStart) Nothing ni
        body' = map CBlockStmt [lblStart] ++ body ++ map CBlockStmt [if_, lblEnd]
      in
        CCompound [] body' ni

    desugarWhile _ o = $abort $ unexp o

    desugarFor ids (CFor init cond incr block ni) = -- for loop with expression {{{2
      let
        ((lblStart, lblEnd), (gotoStart, gotoEnd)) = createLabels ids ni
        body = replaceGotoContinue lblStart lblEnd $ extractBody block
        exprStmt expr = CExpr (Just expr) ni
        init' = case init of
          Left Nothing -> Nothing
          Left (Just e) -> Just $ CBlockStmt $ exprStmt e
          Right d -> Just $ CBlockDecl $ d
        incr' = fmap exprStmt incr
        if_ = fmap (\c -> CIf (CUnary CNegOp c ni) (wrapInBlock gotoEnd) Nothing ni) cond
        body' = 
             init' ?: 
             map CBlockStmt (lblStart : if_ ?: []) 
          ++ body
          ++ map CBlockStmt (incr' ?: gotoStart : lblEnd : [])
      in
        CCompound [] body' ni

    desugarFor _ o = $abort $ unexp o

explicit_return :: CFunctionDef ENodeInfo -> CFunctionDef ENodeInfo -- {{{1
explicit_return o@(CFunDef x1 x2 x3 (CCompound x4 body x6) x7)
  | isVoid (return_type_fd o) && needExplicitReturn body
    = CFunDef x1 x2 x3 (CCompound x4 body' x6) x7
  | otherwise = o
  where
    isVoid (CVoidType _, _) = True
    isVoid _ = False
    needExplicitReturn b= null b || (not . isReturn . last) b
    body' = body ++ [CBlockStmt (CReturn Nothing un)]
    isReturn (CBlockStmt (CReturn _ _)) = True
    isReturn _ = False

explicit_return o = o

wrap_dangling_statements :: CFunctionDef ENodeInfo -> CFunctionDef ENodeInfo -- {{{1
wrap_dangling_statements = everywhere $ mkT dsStat
  where
    dsStat :: CStatement ENodeInfo -> CStatement ENodeInfo
    dsStat (CWhile x1 s x2 x3) = CWhile x1 (wrapInBlock s) x2 x3
    dsStat (CFor x1 x2 x3 s x4) = CFor x1 x2 x3 (wrapInBlock s) x4
    dsStat (CSwitch x1 s x2) = CSwitch x1 (wrapInBlock s) x2
    dsStat (CIf x1 s1 s2 x2) = CIf x1 (wrapInBlock s1) (fmap wrapInBlock s2) x2
    dsStat o = o

unlist_declarations :: CFunctionDef ENodeInfo -> CFunctionDef ENodeInfo -- {{{1
unlist_declarations = undefined
-- unlist_declarations = everywhere (mkT trCompound)
--   where
--   trBlockItem (CBlockDecl decl) = map CBlockDecl (unlistDecl decl)
--   trBlockItem o@(CBlockStmt (CFor (Right decl) y1 y2 y3 y4))
--     | containsCriticalCall decl = map CBlockDecl (unlistDecl decl) ++ [CBlockStmt (CFor (Left Nothing) y1 y2 y3 y4)]
--     | otherwise = [o]
--   trBlockItem o = [o]

--   trCompound (CCompound x1 items x2) = CCompound x1 (concatMap trBlockItem items) x2
--   trCompound x = x

-- utils {{{1

replaceGotoContinue :: CStatement ENodeInfo -> CStatement ENodeInfo -> [CCompoundBlockItem ENodeInfo] -> [CCompoundBlockItem ENodeInfo]
replaceGotoContinue lblStart lblEnd items = everywhereBut (mkQ False blocks) (mkT go) items
  where
    blocks :: CStatement ENodeInfo -> Bool
    blocks (CSwitch _ _ _) = True
    blocks (CWhile _ _ _ _) = True
    blocks (CFor _ _ _ _ _) = True
    blocks _ = False
    go :: CStatement ENodeInfo -> CStatement ENodeInfo
    go (CBreak ni) = CGoto (lblIdent lblEnd) ni
    go (CCont ni) = CGoto (lblIdent lblStart) ni
    go o = o
    lblIdent (CLabel i _ _ _) = i

bfmap :: ([CCompoundBlockItem a] -> [CCompoundBlockItem a]) -> CStatement a -> CStatement a
bfmap f (CCompound x1 items x2) = CCompound x1 (f items) x2
bfmap _ o = $abort $ unexp o

append :: CStatement a -> CStatement a -> CStatement a
append body stmt = bfmap (++[CBlockStmt stmt]) body

prepend :: CStatement a -> CStatement a -> CStatement a
prepend stmt body = bfmap (CBlockStmt stmt:) body

append' :: CStatement a -> Maybe (CStatement a) -> CStatement a
append' body Nothing = body
append' body (Just o) = append body o

prepend' :: Maybe (CStatement a) -> CStatement a -> CStatement a
prepend' Nothing body = body
prepend' (Just o) body = prepend o body

wrapInBlock :: CStatement ENodeInfo -> CStatement ENodeInfo
wrapInBlock o@(CCompound _ _ _) = o
wrapInBlock s = CCompound [] [CBlockStmt s] un

unlistDecl :: CDeclaration ENodeInfo -> [CDeclaration ENodeInfo]
unlistDecl (CDecl x [] z) = [CDecl x [] z] -- struct S { int i; };
unlistDecl (CDecl x ds z) = map (\y -> CDecl x [y] z) ds

nextIds :: State Int (Int, Int)
nextIds = do
  idx <- get
  modify (+2)
  return $ (idx, idx+1)

-- containsCriticalCall o = -- {{{2
--   getAny $ everything mappend (mkQ (Any False) isCriticalCall) o
--   where
--     isCriticalCall :: CExpression ENodeInfo -> Any
--     isCriticalCall (CCall (CVar name _) _ _) = Any $ is_critical cg (symbol name)
--     isCriticalCall _ = Any False
