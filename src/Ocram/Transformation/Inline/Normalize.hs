{-# LANGUAGE TemplateHaskell #-}
module Ocram.Transformation.Inline.Normalize
-- export {{{1
(
  normalize
) where

-- import {{{1
import Control.Monad ((<=<))
import Control.Monad.State (runState, evalState, get, put, State)
import Data.Generics (everything, mkQ, everywhere, mkT, everywhereM, mkM)
import Data.Monoid (Any(Any, getAny), mappend)
import Language.C.Data.Node (nodeInfo)
import Language.C.Syntax.AST
import Language.C.Syntax.Constants (cInteger)
import Ocram.Analysis (is_critical)
import Ocram.Query (return_type)
import Ocram.Symbols (symbol, Symbol)
import Ocram.Transformation.Inline.Names (tempVar)
import Ocram.Transformation.Inline.Types (Transformation)
import Ocram.Transformation.Util (ident, un, map_critical_functions)
import Ocram.Types (Ast)
import Ocram.Util (abort, fromJust_s)
import qualified Data.Traversable as T

normalize :: Transformation -- {{{1
normalize cg ast = return $ unlistGlobalDeclarations $ map_critical_functions cg ast trCriticalFunction
  where
    trCriticalFunction = normalizeStatements . deferCriticalInitializations . unlistDeclarations . wrapDanglingStatements

    wrapDanglingStatements = everywhere $ mkT dsStat -- {{{2
      where
        dsStat (CWhile x1 s x2 x3) = CWhile x1 (wrapInBlock s) x2 x3
        dsStat (CFor x1 x2 x3 s x4) = CFor x1 x2 x3 (wrapInBlock s) x4
        dsStat (CSwitch x1 s x2) = CSwitch x1 (wrapInBlock s) x2
        dsStat (CIf x1 s1 s2 x2) = CIf x1 (wrapInBlock s1) (fmap wrapInBlock s2) x2
        dsStat o = o

        wrapInBlock o@(CCompound _ _ _) = o
        wrapInBlock s = CCompound [] [CBlockStmt s] (nodeInfo s)

    unlistDeclarations = transformCompound trBlockItem -- {{{2
      where
      trBlockItem (CBlockDecl decl) = map CBlockDecl (unlistDecl decl)
      trBlockItem o@(CBlockStmt (CFor (Right decl) y1 y2 y3 y4))
        | containsCriticalCall decl = map CBlockDecl (unlistDecl decl) ++ [CBlockStmt (CFor (Left Nothing) y1 y2 y3 y4)]
        | otherwise = [o]
      trBlockItem o = [o]

    transformCompound trBlockItem = everywhere (mkT trCompound)
      where
      trCompound (CCompound x1 items x2) = CCompound x1 (concatMap trBlockItem items) x2
      trCompound x = x


    deferCriticalInitializations = transformCompound trBlockItem -- {{{2
      where
      trBlockItem o@(CBlockDecl (CDecl y1 [(Just declr, Just (CInitExpr cexpr _), y2)] y3))
        | containsCriticalCall cexpr = [declare, initialize]
        | otherwise = [o]
        where
          declare = CBlockDecl (CDecl y1 [(Just declr, Nothing, y2)] y3)
          initialize = CBlockStmt (CExpr (Just (CAssign CAssignOp (CVar (ident (symbol declr)) un) cexpr un)) un)
      trBlockItem o = [o]

    normalizeStatements (CFunDef x1 x2 x3 s x4) = CFunDef x1 x2 x3 (evalState (trBlock s) 0) x4 -- {{{2
      where
        trBlock :: CStat -> State Int CStat
        trBlock (CCompound y1 items y2) = do
          items' <- mapM trBlockItem items
          return $ CCompound y1 (concat items') y2
        trBlock _ = $abort "unexpected parameters"

        -- critical calls in return expressions -- {{{3
        trBlockItem o@(CBlockStmt (CReturn (Just cexpr) ni))
          | containsCriticalCall cexpr = do
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
          if containsCriticalCall condition
            then do
              (condition', extraDecls) <- extractCriticalCalls condition
              return $ extraDecls ++ [CBlockStmt (CIf condition' thenBlock' elseBlock' ni)]
            else
              return [CBlockStmt (CIf condition thenBlock' elseBlock' ni)]

        -- critical calls in conditions of while loops -- {{{3
        trBlockItem (CBlockStmt (CWhile condition body False ni)) = do
          body' <- trBlock body
          o <- if containsCriticalCall condition
            then do
              (condition', extraDecls) <- extractCriticalCalls condition
              let body'' = (extraDecls ++ breakIf condition') `prepend` body'
              return $ CWhile trueCondition body'' False ni
            else
              return $ CWhile condition body' False ni
          return [CBlockStmt o]

        -- critical calls in conditions of do loops -- {{{3
        trBlockItem (CBlockStmt (CWhile condition body True ni)) = do
          body' <- trBlock body
          o <- if containsCriticalCall condition
            then do
              (condition', extraDecls) <- extractCriticalCalls condition
              let body'' = body' `append` (extraDecls ++ breakIf condition')
              return $ CWhile trueCondition body'' True ni
            else
              return $ CWhile condition body' True ni
          return [CBlockStmt o]

        -- critical calls in for loops -- {{{3
        trBlockItem (CBlockStmt o@(CFor (Left Nothing) _ _ _ _)) = do
          (CFor _ condition loopExpr body ni) <- trForCommon o
          let o' = CFor (Left Nothing) condition loopExpr body ni
          return [CBlockStmt o']

        trBlockItem (CBlockStmt o@(CFor (Left (Just cexpr)) _ _ _ _)) = do
          (CFor _ condition loopExpr body ni) <- trForCommon o
          if containsCriticalCall cexpr
            then
              let stmt = CBlockStmt $ CFor (Left Nothing) condition loopExpr body ni in
              if isInNormalForm cexpr
                then return $ exprStmt cexpr ++ [stmt]
              else do
                (cexpr', extraDecls) <- extractCriticalCalls cexpr
                return $ extraDecls ++ exprStmt cexpr' ++ [stmt]
            else
              let stmt = CBlockStmt $ CFor (Left (Just cexpr)) condition loopExpr body ni in
              return [stmt]

        trBlockItem (CBlockStmt o@(CFor (Right _) _ _ _ _)) = do
          o' <- trForCommon o
          return [CBlockStmt o']

        -- default case -- {{{3
        trBlockItem (CBlockStmt o@(CCompound _ _ _)) = trBlock o >>= return . (:[]) . CBlockStmt
        trBlockItem o = return [o]

        -- transformers for loop expressions -- {{{2
        trForCommon = trForLoopExpr <=< trForCondition <=< trForBlock

        trForBlock (CFor y1 y2 y3 body ni) = do
          body' <- trBlock body
          return $ CFor y1 y2 y3 body' ni
        trForBlock _ = $abort "unexpected parameters"

        trForCondition o@(CFor _ Nothing _ _ _) = return o
        trForCondition o@(CFor y1 (Just condition) y3 body ni) = do
          (condition', extraDecls) <- extractCriticalCalls condition
          let o' = CFor y1 Nothing y3 (body `append` (extraDecls ++ breakIf condition')) ni
          return $ if null extraDecls then o else o'
        trForCondition _ = $abort "unexpected parameters"

        trForLoopExpr o@(CFor _ _ Nothing _ _) = return o
        trForLoopExpr o@(CFor y1 y2 (Just loopExpr) body ni) = do
          (loopExpr', extraDecls) <- extractCriticalCalls loopExpr
          let o' = CFor y1 y2 Nothing (body `append` (extraDecls ++ exprStmt loopExpr')) ni
          return $ if null extraDecls then o else o'
        trForLoopExpr _ = $abort "unexpected parameters"


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
                  put (idx + 1, decl : decls)
                  return $ CVar (ident (symbol decl)) un
              | otherwise = return o
            trCriticalCall o = return o

            newDecl :: CExpr -> Symbol -> Int -> CDecl
            newDecl call callee tmpVarIdx =
              CDecl [CTypeSpec returnType] [(Just declarator, Just initializer, Nothing)] un
              where
                (returnType, dds) = $fromJust_s $ return_type ast callee
                declarator = CDeclr (Just tmpVar) dds Nothing [] un
                initializer = CInitExpr call un
                tmpVar = ident (tempVar tmpVarIdx)

    containsCriticalCall o = -- {{{2
      getAny $ everything mappend (mkQ (Any False) isCriticalCall) o
      where
        isCriticalCall :: CExpr -> Any
        isCriticalCall (CCall (CVar name _) _ _) = Any $ is_critical cg (symbol name)
        isCriticalCall _ = Any False

unlistGlobalDeclarations :: Ast -> Ast -- {{{2
unlistGlobalDeclarations (CTranslUnit ds ni) = CTranslUnit (foldr unlist [] ds) ni
  where
  unlist (CDeclExt d@(CDecl _ _ _)) xs = map CDeclExt (unlistDecl d) ++ xs
  unlist x xs = x : xs

-- utils -- {{{1
isInNormalForm :: CExpr -> Bool
isInNormalForm (CCall _ _ _) = True
isInNormalForm (CAssign CAssignOp _ (CCall _ _ _) _) = True
isInNormalForm _ = False

prepend :: [CBlockItem] -> CStat -> CStat
prepend items' (CCompound x1 items x2) = CCompound x1 (items' ++ items) x2
prepend _ _ = $abort "unexpected parameters"

append :: CStat -> [CBlockItem] -> CStat
append (CCompound x1 items x2) items' = CCompound x1 (items ++ items') x2
append _ _ = $abort "unexpected parameters"

breakIf :: CExpr -> [CBlockItem]
breakIf condition = [CBlockStmt $ CIf (CUnary CNegOp condition un) (CCompound [] [CBlockStmt (CBreak un)] un) Nothing un]

exprStmt :: CExpr -> [CBlockItem]
exprStmt cexpr = [CBlockStmt $ CExpr (Just cexpr) un]

trueCondition :: CExpr
trueCondition = CConst (CIntConst (cInteger 1) un)

unlistDecl :: CDecl -> [CDecl]
unlistDecl (CDecl x [] z) = [CDecl x [] z] -- struct S { int i; };
unlistDecl (CDecl x ds z) = map (\y -> CDecl x [y] z) ds
