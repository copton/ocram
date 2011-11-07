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


    rewriteConditions (CFunDef x1 x2 x3 s x4) =
      CFunDef x1 x2 x3 (trBlock s) x4
      where
        trBlock (CCompound y1 items y2) = CCompound y1 (foldr trBlockItem [] items) y2
        trBlock _ = $abort "unexpected parameters"
      
        trBlockItem (CBlockStmt (CIf condition thenBlock elseBlock ni)) items =
          let 
            (condition', extraDecls) = extractCriticalCalls condition
            o = CBlockStmt $ CIf condition' (trBlock thenBlock) (fmap trBlock elseBlock) ni
          in
            extraDecls ++ [o] ++ items
          
        trBlockItem o items = o : items

        extractCriticalCalls cexp = second (map CBlockDecl) $ runState (everywhereM (mkM trCriticalCall) cexp) []

        trCriticalCall o@(CCall (CVar name _) _ _)
          | is_critical cg (symbol name) = do
              decls <- get
              let decl = newDecl o (symbol name) (length decls)
              put $ decl : decls
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
