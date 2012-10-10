{-# LANGUAGE TemplateHaskell #-}
module Ocram.Intermediate.NormalizeCriticalCalls
-- exports {{{1
(
  normalize_critical_calls
) where

-- imports {{{1
import Data.Generics (everywhereM, mkM)
import Control.Monad (liftM)
import Control.Monad.State (State, runState, put, get, modify)
import Language.C.Syntax.AST
import Language.C.Data.Ident (internalIdent, Ident)
import Language.C.Data.Node (undefNode)
import Ocram.Debug (CStat', CExpr', eun, aset)
import Ocram.Intermediate.Representation (Variable(..))
import Ocram.Names (varCrit)
import Ocram.Symbols (Symbol, symbol)
import Ocram.Util (fromJust_s, abort, unexp)
import Prelude hiding (init)

import qualified Data.Map as M

normalize_critical_calls :: M.Map Symbol (CTypeSpec, [CDerivedDeclr]) -> [CStat'] -> ([CStat'], [Variable]) -- {{{1
normalize_critical_calls sf items =
  let (items', Ctx vars _ _) = runState (mapM tStmt items) (Ctx [] 0 [])
  in (concat items', vars)
  where
    tStmt :: CStat' -> S [CStat'] -- {{{2
    tStmt o@(CIf cond t e ni) =
      keepOrReplace [cond] o (\[cond'] -> CIf cond' t e ni) 

    tStmt o@(CReturn (Just expr) ni) =
      keepOrReplace [expr] o (\[expr'] -> CReturn (Just expr') ni)

    tStmt o@(CExpr (Just expr) ni)
      | isInNormalForm expr = return [o]
      | otherwise = keepOrReplace [expr] o (\[expr'] -> CExpr (Just expr') ni)

    tStmt o@(CExpr Nothing _)                = return [o]
    tStmt o@(CGoto _ _)                      = return [o]
    tStmt o@(CLabel _ (CExpr Nothing _) _ _) = return [o]
    tStmt o@(CReturn Nothing _)              = return [o]
    tStmt x                                  = $abort $ unexp x

    keepOrReplace :: [CExpr'] -> CStat' -> ([CExpr'] -> CStat') -> S [CStat'] -- {{{2
    keepOrReplace es s f = do
      (inits, es') <- liftM unzip $ mapM extractCriticalCalls es
      let allInits = concat inits
      if null allInits
        then return [s]
        else
          let s' = f es' in
          return $ map (aset (annotation s)) $ allInits ++ [s']

    isInNormalForm (CCall _ _ _)                 = True -- {{{2
    isInNormalForm (CAssign _ _ (CCall _ _ _) _) = True
    isInNormalForm _                             = False

    extractCriticalCalls :: CExpr' -> S ([CStat'], CExpr') -- {{{2
    extractCriticalCalls expr = do
      modify (\(Ctx v c _) -> Ctx v c [])
      expr' <- everywhereM (mkM tExpr) expr
      (Ctx _ _ inits) <- get
      return (inits, expr')
      
    tExpr :: CExpr' -> S CExpr' -- {{{2
    tExpr call@(CCall (CVar callee _) _ _)
      | M.member (symbol callee) sf = do
          Ctx vars count inits <- get
          let
            decl       = newDecl callee count
            ni         = annotation call
            name       = symbol decl
            init       = CExpr (Just (CAssign CAssignOp (CVar (internalIdent name) eun) call ni)) ni
            var        = EVariable decl
          put $ Ctx (var : vars) (count + 1) (init : inits)
          return $ CVar (internalIdent name) eun

      | otherwise = return call
  
    tExpr o = return o

    newDecl :: Ident -> Int -> CDecl -- {{{2
    newDecl callee count =
      CDecl [CTypeSpec returnType] [(Just declarator, Nothing, Nothing)] undefNode
      where
        (returnType, dds) = $fromJust_s $ M.lookup (symbol callee) sf
        tmpVar            = internalIdent (varCrit count)
        declarator        = CDeclr (Just tmpVar) dds Nothing [] undefNode

data Ctx = Ctx { -- {{{2
    ctxVars  :: [Variable]
  , ctxCount :: Int
  , ctxInits :: [CStat']
  }

type S a = State Ctx a -- {{{2
