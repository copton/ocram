{-# LANGUAGE TemplateHaskell #-}
module Ocram.Transformation.Inline.Normalize
-- export {{{1
(
  normalize
) where

-- import {{{1
import Control.Arrow (second)
import Control.Monad.State (runState, get, put)
import Data.Generics (everywhere, mkT, everywhereM, mkM)
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

    wrapDanglingStatements = everywhere $ mkT dsStat
      where
        dsStat (CWhile x1 s x2 x3) = CWhile x1 (wrapInBlock s) x2 x3
        dsStat (CFor x1 x2 x3 s x4) = CFor x1 x2 x3 (wrapInBlock s) x4
        dsStat (CSwitch x1 s x2) = CSwitch x1 (wrapInBlock s) x2
        dsStat (CIf x1 s1 s2 x2) = CIf x1 (wrapInBlock s1) (fmap wrapInBlock s2) x2
        dsStat o = o

        wrapInBlock o@(CCompound _ _ _) = o
        wrapInBlock s = CCompound [] [CBlockStmt s] (nodeInfo s)


    rewriteConditions (CFunDef x1 x2 x3 s x4) = CFunDef x1 x2 x3 (trBlock s) x4
      where
        trBlock (CCompound y1 items y2) = CCompound y1 (fst $ foldr trBlockItem ([], 0) items) y2
        trBlock _ = $abort "unexpected parameters"
        
        -- this already is in normal form, so don't touch it. -- {{{2
        trBlockItem o@(CBlockStmt (CExpr (Just (CCall _ _ _)) _)) (items, idx) = (o : items, idx)
        trBlockItem o@(CBlockStmt (CExpr (Just (CAssign CAssignOp _ (CCall _ _ _) _)) _)) (items, idx) = (o : items, idx)
      
        -- critical calls in conditions of if statements -- {{{2
        trBlockItem (CBlockStmt (CIf condition thenBlock elseBlock ni)) (items, idx) =
          let 
            (condition', extraDecls) = extractCriticalCalls idx condition
            o = CBlockStmt $ CIf condition' (trBlock thenBlock) (fmap trBlock elseBlock) ni
          in
            (extraDecls ++ [o] ++ items, idx + length extraDecls)


        trBlockItem (CBlockStmt (CExpr (Just cexpr) ni)) (items, idx) =
          let
            (cexpr', extraDecls) = extractCriticalCalls idx cexpr
            o = CBlockStmt $ CExpr (Just cexpr') ni
          in
            (extraDecls ++ [o] ++ items, idx + length extraDecls)

        -- critical calls in conditions of while loops -- {{{2
        trBlockItem o@(CBlockStmt (CWhile condition body False ni)) (items, idx) =
          let
            (condition', extraDecls) = extractCriticalCalls idx condition
            body' = prependBody (extraDecls ++ [breakIf condition']) (trBlock body)
            o' = CBlockStmt $ CWhile trueCondition body' False ni
          in
            if not (null extraDecls)
              then (o' : items, idx + length extraDecls)
              else (o : items, idx)

        -- critical calls in conditions of do loops -- {{{2
        trBlockItem o@(CBlockStmt (CWhile condition body True ni)) (items, idx) =
          let
            (condition', extraDecls) = extractCriticalCalls idx condition
            body' = appendBody (trBlock body) (extraDecls ++ [breakIf condition']) 
            o' = CBlockStmt $ CWhile trueCondition body' True ni
          in
            if not (null extraDecls)
              then (o' : items, idx + length extraDecls)
              else (o : items, idx)

        -- critical calls in condition of for loops -- {{{2
        -- TODO for-loops are more complicated because there are four different cases of possible critical calls.
        trBlockItem o@(CBlockStmt (CFor y1 (Just condition) y2 body ni)) (items, idx) =
          let
            (condition', extraDecls) = extractCriticalCalls idx condition
            body' = appendBody (trBlock body) (extraDecls ++ [breakIf condition'])
            o' = CBlockStmt $ CFor y1 Nothing y2 body' ni
          in
            if not (null extraDecls)
              then (o' : items, idx + length extraDecls)
              else (o : items, idx)
          
        -- default case -- {{{2
        trBlockItem o (items, idx) = (o : items, idx)

        extractCriticalCalls idx cexp = -- {{{2
          second (map CBlockDecl) $ runState (everywhereM (mkM (trCriticalCall idx)) cexp) []
          where
            trCriticalCall idxOffset o@(CCall (CVar name _) _ _)
              | is_critical cg (symbol name) = do
                  decls <- get
                  let decl = newDecl o (symbol name) (idxOffset + length decls)
                  put $ decl : decls
                  return $ CVar (ident (symbol decl)) un
              | otherwise = return o
            trCriticalCall _ o = return o

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
