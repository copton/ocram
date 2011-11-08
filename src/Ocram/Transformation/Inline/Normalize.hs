{-# LANGUAGE TemplateHaskell #-}
module Ocram.Transformation.Inline.Normalize
-- export {{{1
(
  normalize
) where

-- import {{{1
import Control.Monad (foldM)
import Control.Monad.State (runState, evalState, get, put, State)
import Data.Generics (everywhere, mkT, everywhereM, mkM)
import qualified Data.Traversable as T
import Language.C.Data.Node (nodeInfo)
import Language.C.Syntax.AST
import Language.C.Syntax.Constants (cInteger)
import Ocram.Analysis (is_critical)
import Ocram.Query (return_type)
import Ocram.Symbols (symbol, Symbol)
import Ocram.Transformation.Util (ident, un)
import Ocram.Transformation.Inline.Names (tempVar)
import Ocram.Transformation.Inline.Types (Transformation)
import Ocram.Transformation.Util (map_critical_functions)
import Ocram.Util (abort, fromJust_s)

normalize :: Transformation -- {{{1
normalize cg ast = return $ map_critical_functions cg ast trCriticalFunction
  where
    trCriticalFunction = rewriteConditions . wrapDanglingStatements

    wrapDanglingStatements = everywhere $ mkT dsStat -- {{{2
      where
        dsStat (CWhile x1 s x2 x3) = CWhile x1 (wrapInBlock s) x2 x3
        dsStat (CFor x1 x2 x3 s x4) = CFor x1 x2 x3 (wrapInBlock s) x4
        dsStat (CSwitch x1 s x2) = CSwitch x1 (wrapInBlock s) x2
        dsStat (CIf x1 s1 s2 x2) = CIf x1 (wrapInBlock s1) (fmap wrapInBlock s2) x2
        dsStat o = o

        wrapInBlock o@(CCompound _ _ _) = o
        wrapInBlock s = CCompound [] [CBlockStmt s] (nodeInfo s)


    rewriteConditions (CFunDef x1 x2 x3 s x4) = CFunDef x1 x2 x3 (evalState (trBlock s) 0) x4 -- {{{2
      where
        trBlock :: CStat -> State Int CStat
        trBlock (CCompound y1 items y2) = do
          items' <- foldM trBlockItem [] items
          return $ CCompound y1 (reverse items') y2
        trBlock _ = $abort "unexpected parameters"
        
        -- this already is in normal form, so don't touch it. -- {{{3
        trBlockItem items o@(CBlockStmt (CExpr (Just (CCall _ _ _)) _)) = return $ o : items
        trBlockItem items o@(CBlockStmt (CExpr (Just (CAssign CAssignOp _ (CCall _ _ _) _)) _)) = return (o : items)

        -- critical calls in nested expressions -- {{{3
        trBlockItem items (CBlockStmt (CExpr (Just cexpr) ni)) = do
          (cexpr', extraDecls) <- extractCriticalCalls cexpr
          let o = CBlockStmt $ CExpr (Just cexpr') ni
          return $ o : extraDecls ++ items

        -- critical calls in conditions of if statements -- {{{3
        trBlockItem items (CBlockStmt (CIf condition thenBlock elseBlock ni)) = do
          (condition', extraDecls) <- extractCriticalCalls condition
          thenBlock' <- trBlock thenBlock
          elseBlock' <- T.mapM trBlock elseBlock
          let o = CBlockStmt $ CIf condition' thenBlock' elseBlock' ni
          return $ o : extraDecls ++ items

        -- critical calls in conditions of while loops -- {{{3
        trBlockItem items o@(CBlockStmt (CWhile condition body False ni)) = do
          (condition', extraDecls) <- extractCriticalCalls condition
          body' <- trBlock body
          let body'' = prependBody (extraDecls ++ [breakIf condition']) body'
          let o' = CBlockStmt $ CWhile trueCondition body'' False ni
          return $ if null extraDecls
            then o : items
            else o' : items

        -- critical calls in conditions of do loops -- {{{3
        trBlockItem items o@(CBlockStmt (CWhile condition body True ni)) = do
          (condition', extraDecls) <- extractCriticalCalls condition
          body' <- trBlock body
          let body'' = appendBody body' (extraDecls ++ [breakIf condition']) 
          let o' = CBlockStmt $ CWhile trueCondition body'' True ni
          return $ if null extraDecls
            then o : items
            else o' : items

        -- critical calls in for loops -- {{{3
--        trBlockItem (CBlockStmt o@(CFor (Left Nothing) condition loopExpr body ni) (items, idx) =
--          let
--            o' = CFor (Left Nothing) condition loopExpr (trBlock body) ni
--            (o'', idx') = trForLoopExpr . trForCondition $ (o', idx)
--          in
--            (o'' : items, idx')
--          
--        trBlockItem (CBlockStmt o@(CFor (Left (Just cexpr)) condition loopExpr body ni) (items, idx) =
--          let
--            o' = CFor (Left Nothing) condition loopExpr (trBlock body) ni
--            (o'', idx') = trForLoopExpr . trForCondition $ (o', idx)
--            (condition', extraDecls) = extractCriticalCalls idx' 
--            
--          in
--            if idx' == idx
--              then (o' : items, idx)
--              else (o'' : items, idx')
          
        -- default case -- {{{3
        trBlockItem items o = return $ o : items

        -- transformers for loop expressions -- {{{2
--        trForLoopExpr t@(CFor _ _ Nothing _, _) = t
--        trForLoopExpr t@(CFor y1 y2 (Just loopExpr) body ni, idx) =
--          let
--            (loopExpr', extraDecls) = extractCriticalCalls idx
--            body' = appendBody body (extraDecls ++ loopExpr') 
--            o' = CFor y1 y2 Nothing body' ni
--          in
--            if null extraDecls
--              then t
--              else (o', idx + length extraDecls)
--
--        trForCondition t@(CFor _ Nothing _ _, _) = t
--        trForCondition t@(CFor y1 (Just condition) y3 body ni, idx) =
--          let
--            (condition', extraDecls) = extractCriticalCalls idx
--            body' = appendBody body (extraDecls ++ [breakIf condition'])
--            o' = CFor y1 Nothing y3 body' ni
--          in
--            if null extraDecls
--              then t
--              else (o', idx + length extraDecls)

        extractCriticalCalls cexp = do -- {{{2
          idx <- get
          let (cexp', (idx', decls)) = runState (everywhereM (mkM trCriticalCall) cexp) (idx, [])
          put idx'
          return (cexp', map CBlockDecl decls)
        
          where
            trCriticalCall o@(CCall (CVar name _) _ _)
              | is_critical cg (symbol name) = do
                  (idx, decls) <- get
                  let decl = newDecl o (symbol name) idx
                  put $ (idx + 1, decl : decls)
                  return $ CVar (ident (symbol decl)) un
              | otherwise = return o
            trCriticalCall o = return o

            newDecl :: CExpr -> Symbol -> Int -> CDecl
            newDecl call callee tmpVarIdx =
              CDecl [returnType] [(Just declarator, Just initializer, Nothing)] un
              where
                returnType = CTypeSpec $ $fromJust_s $ return_type ast callee
                declarator = CDeclr (Just tmpVar) [] Nothing [] un
                initializer = CInitExpr call un
                tmpVar = ident (tempVar tmpVarIdx)

-- utils -- {{{1
prependBody :: [CBlockItem] -> CStat -> CStat
prependBody items' (CCompound x1 items x2) = CCompound x1 (items' ++ items) x2
prependBody _ _ = $abort "unexpected parameters"

appendBody :: CStat -> [CBlockItem] -> CStat
appendBody (CCompound x1 items x2) items' = CCompound x1 (items ++ items') x2
appendBody _ _ = $abort "unexpected parameters"

breakIf :: CExpr -> CBlockItem
breakIf condition = CBlockStmt $ CIf (CUnary CNegOp condition un) (CCompound [] [CBlockStmt (CBreak un)] un) Nothing un

trueCondition :: CExpr
trueCondition = CConst (CIntConst (cInteger 1) un)
