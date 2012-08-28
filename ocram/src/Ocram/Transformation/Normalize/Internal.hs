{-# LANGUAGE TemplateHaskell #-}
module Ocram.Transformation.Normalize.Internal
-- export {{{1
(
  desugar_control_structures, explicit_return, wrap_dangling_statements, unlist_declarations, defer_critical_initialization, critical_statements
) where

-- import {{{1
import Control.Monad.State (runState, put, evalState, State, get, modify)
import Data.Data (Data)
import Data.Generics (everything, mkQ, everywhere, everywhereBut, mkT, everywhereM, mkM)
import Data.Monoid (Any(Any, getAny), mappend)
import Language.C.Syntax.AST
import Ocram.Debug (ENodeInfo(..))
import Ocram.Query (return_type_fd, return_type)
import Ocram.Symbols (symbol, Symbol)
import Ocram.Transformation.Names (ctrlbl, varCrit)
import Ocram.Transformation.Util (ident)
import Ocram.Transformation.Types
import Ocram.Util (abort, unexp, tmap, (?:), fromJust_s)
import Prelude hiding (init)

import qualified Data.Set as Set
import qualified Data.Traversable as T

explicit_return :: CFunDef' -> CFunDef' -- {{{1
explicit_return o@(CFunDef x1 x2 x3 (CCompound x4 body x6) ni)
  | isVoid (return_type_fd o) && needExplicitReturn body
    = CFunDef x1 x2 x3 (CCompound x4 body' x6) ni
  | otherwise = o
  where
    isVoid (CVoidType _, _) = True
    isVoid _ = False
    needExplicitReturn b= null b || (not . isReturn . last) b
    body' = body ++ [CBlockStmt (CReturn Nothing ni)]
    isReturn (CBlockStmt (CReturn _ _)) = True
    isReturn _ = False

explicit_return o = o

desugar_control_structures :: Int -> CFunDef' -> CFunDef' -- {{{1
desugar_control_structures tid (CFunDef x1 x2 x3 (CCompound y1 items y2) x4) =
  CFunDef x1 x2 x3 (CCompound y1 items' y2) x4
  where
    items' = evalState (everywhereM (mkM go) items) 0

    desugarWhile ids (CWhile cond block False ni) = -- while loop {{{2
      let
        ((lblStart, lblEnd), (gotoStart, gotoEnd)) = createLabels ids ni
        body = replaceGotoContinue lblStart lblEnd $ extractBody block
        cond' = CUnary CNegOp cond ni
        if_ = CIf cond' gotoEnd Nothing ni
        body' = map CBlockStmt [lblStart, if_] ++ body ++ map CBlockStmt [gotoStart, lblEnd]
      in
        CCompound [] body' ni

    desugarWhile ids (CWhile cond block True ni) = -- do loop {{{2
      let
        ((lblStart, lblEnd), (gotoStart, _)) = createLabels ids ni
        body = replaceGotoContinue lblStart lblEnd $ extractBody block
        if_ = CIf cond gotoStart Nothing ni
        body' = map CBlockStmt [lblStart] ++ body ++ map CBlockStmt [if_, lblEnd]
      in
        CCompound [] body' ni

    desugarWhile _ o = $abort $ unexp o

    desugarFor ids (CFor init cond incr block ni) = -- for loop {{{2
      let
        ((lblStart, lblEnd), (gotoStart, gotoEnd)) = createLabels ids ni
        body = replaceGotoContinue lblStart lblEnd $ extractBody block
        exprStmt expr = CExpr (Just expr) ni
        init' = case init of
          Left Nothing -> Nothing
          Left (Just e) -> Just $ CBlockStmt $ exprStmt e
          Right d -> Just $ CBlockDecl $ d
        incr' = fmap exprStmt incr
        if_ = fmap (\c -> CIf (CUnary CNegOp c ni) gotoEnd Nothing ni) cond
        body' =
             init' ?: 
             map CBlockStmt (lblStart : if_ ?: []) 
          ++ body
          ++ map CBlockStmt (incr' ?: gotoStart : lblEnd : [])
      in
        CCompound [] body' ni

    desugarFor _ o = $abort $ unexp o

    -- utils {{{2
    go :: CStat' -> State Int (CStat')
    go o@(CWhile _ _ _ _) = do
      ids <- nextIds
      return $ desugarWhile ids o

    go o@(CFor _ _ _ _ _) = do
      ids <- nextIds
      return $ desugarFor ids o

    go o = return o

    createLabels ids ni =
      let
        identifiers = tmap (ident . ctrlbl tid) ids
        labels = tmap (\i -> CLabel i (CExpr Nothing ni) [] ni) identifiers
        gotos = tmap (\i -> CGoto i ni) identifiers
      in 
        (labels, gotos)

    extractBody (CCompound _ body _) = body
    extractBody o = [CBlockStmt o]

    nextIds = do
      idx <- get
      modify (+2)
      return $ (idx, idx+1)

    replaceGotoContinue :: CStat' -> CStat' -> [CBlockItem'] -> [CBlockItem'] -- {{{2
    replaceGotoContinue lblStart lblEnd = everywhereBut (mkQ False blocks) (mkT trans)
      where
        blocks :: CStat' -> Bool
        blocks (CSwitch _ _ _) = True
        blocks (CWhile _ _ _ _) = True
        blocks (CFor _ _ _ _ _) = True
        blocks _ = False
        trans :: CStat' -> CStat'
        trans (CBreak ni) = CGoto (lblIdent lblEnd) ni
        trans (CCont ni) = CGoto (lblIdent lblStart) ni
        trans o = o
        lblIdent (CLabel i _ _ _) = i
        lblIdent o = $abort $ unexp o

desugar_control_structures _ o = $abort $ unexp o

wrap_dangling_statements :: CFunDef' -> CFunDef' -- {{{1
wrap_dangling_statements = everywhere $ mkT dsStat
  where
    dsStat :: CStat' -> CStat'
    dsStat (CSwitch x1 s ni) = CSwitch x1 (wrapInBlock ni s) ni
    dsStat (CIf x1 s1 s2 ni) = CIf x1 (wrapInBlock ni s1) (fmap (wrapInBlock ni) s2) ni
    dsStat o@(CWhile _ _ _ _) = $abort $ unexp o
    dsStat o@(CFor _ _ _ _ _) = $abort $ unexp o
    dsStat o = o
    wrapInBlock _ o@(CCompound _ _ _) = o
    wrapInBlock ni s = CCompound [] [CBlockStmt s] ni

unlist_declarations :: CFunDef' -> CFunDef' -- {{{1
unlist_declarations = transformCompound tr
  where
  tr (CBlockDecl decl) = map CBlockDecl (unlistDecl decl)
  tr o = [o]

  unlistDecl (CDecl x [] ni) = [CDecl x [] ni] -- struct S { int i; };
  unlistDecl (CDecl x ds ni) = map (\y -> CDecl x [y] ni) ds

defer_critical_initialization :: Set.Set Symbol -> CFunDef' -> CFunDef' -- {{{1
defer_critical_initialization cf = transformCompound tr
  where
  tr o@(CBlockDecl (CDecl y1 [(Just declr, Just (CInitExpr cexpr _), y2)] y3))
    | containsCriticalCall cf cexpr = [declare, initialize]
    | otherwise = [o]
    where
      declare = CBlockDecl (CDecl y1 [(Just declr, Nothing, y2)] y3)
      initialize = CBlockStmt (CExpr (Just (CAssign CAssignOp (CVar (ident (symbol declr)) ni) cexpr ni)) ni)
      ni = annotation cexpr
  tr o = [o]

critical_statements :: Int -> Set.Set Symbol -> CTranslUnit' -> CFunDef' -> CFunDef' -- {{{1
critical_statements tid cf ast (CFunDef x1 x2 x3 s x4) = CFunDef x1 x2 x3 (evalState (trBlock s) 0) x4
  where
    trBlock :: CStatement ENodeInfo -> State Int (CStatement ENodeInfo)
    trBlock (CCompound y1 items y2) = do
      items' <- mapM trBlockItem items
      return $ CCompound y1 (concat items') y2
    trBlock x = $abort $ unexp x

    -- transformations {{{2
    -- critical calls in return statement -- {{{3
    trBlockItem o@(CBlockStmt (CReturn (Just cexpr) ni))
      | containsCriticalCall cf cexpr = do
          (cexpr', extraDecls) <- extractCriticalCalls cexpr
          let o' = CBlockStmt $ CReturn (Just cexpr') ni
          return $ extraDecls ++ [o']
      | otherwise = return [o]

    -- critical calls in nested expressions -- {{{3
    trBlockItem o@(CBlockStmt (CExpr (Just cexpr) ni))
      | isInNormalForm cexpr = return [o]
      | otherwise = do
          (cexpr', extraDecls) <- extractCriticalCalls cexpr
          let o' = CBlockStmt $ CExpr (Just cexpr') ni
          return $ extraDecls ++ [o']

    -- critical calls in conditions of if statements -- {{{3
    trBlockItem (CBlockStmt (CIf condition thenBlock elseBlock ni)) = do
      thenBlock' <- trBlock thenBlock
      elseBlock' <- T.mapM trBlock elseBlock
      if containsCriticalCall cf condition
        then do
          (condition', extraDecls) <- extractCriticalCalls condition
          return $ extraDecls ++ [CBlockStmt (CIf condition' thenBlock' elseBlock' ni)]
        else
          return [CBlockStmt (CIf condition thenBlock' elseBlock' ni)]

    -- default case -- {{{3
    trBlockItem (CBlockStmt o@(CCompound _ _ _)) = trBlock o >>= return . (:[]) . CBlockStmt
    trBlockItem o = return [o]

    extractCriticalCalls cexp = do -- {{{2
      idx <- get
      let (cexp', (idx', decls)) = runState (everywhereM (mkM trCriticalCall) cexp) (idx, [])
      put idx'
      return (cexp', map CBlockDecl decls)

      where
        trCriticalCall o@(CCall (CVar name _) _ ni)
          | Set.member (symbol name) cf = do
              (idx, decls) <- get
              let decl = newDecl o (symbol name) idx
              put (idx + 1, decl : decls)
              return $ CVar (ident (symbol decl)) ni
          | otherwise = return o
        trCriticalCall o = return o

        newDecl :: CExpression ENodeInfo -> Symbol -> Int -> CDeclaration ENodeInfo
        newDecl call callee tmpVarIdx =
          CDecl [CTypeSpec returnType] [(Just declarator, Just initializer, Nothing)] ni
          where
            (returnType, dds) = $fromJust_s $ return_type ast callee
            declarator = CDeclr (Just tmpVar) dds Nothing [] ni
            initializer = CInitExpr call ni
            tmpVar = ident (varCrit tid tmpVarIdx)
            ni = annotation call

    isInNormalForm :: CExpression ENodeInfo -> Bool
    isInNormalForm (CCall _ _ _) = True
    isInNormalForm (CAssign CAssignOp _ (CCall _ _ _) _) = True
    isInNormalForm _ = False

-- utils {{{1
containsCriticalCall :: Data a => Set.Set Symbol -> a -> Bool
containsCriticalCall cf o = -- {{{2
  getAny $ everything mappend (mkQ (Any False) isCriticalCall) o
  where
    isCriticalCall :: CExpr' -> Any
    isCriticalCall (CCall (CVar name _) _ _) = Any $ Set.member (symbol name) cf
    isCriticalCall _ = Any False

transformCompound :: (CBlockItem' -> [CBlockItem']) -> CFunDef' -> CFunDef'
transformCompound f = everywhere (mkT trans)
  where
  trans (CCompound x1 items x2) = CCompound x1 (concatMap f items) x2
  trans x = x
