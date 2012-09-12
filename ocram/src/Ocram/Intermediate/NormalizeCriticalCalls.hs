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
import Language.C.Data.Node (undefNode)
import Language.C.Data.Ident (internalIdent, Ident)
import Ocram.Intermediate.Representation (Variable(..))
import Ocram.Names (varCrit)
import Ocram.Symbols (Symbol, symbol)
import Ocram.Util (fromJust_s, abort, unexp)
import Ocram.Query (return_type_fd)
import Prelude hiding (init)

import qualified Data.Map as M

type CriticalFunctions = M.Map Symbol CFunDef

normalize_critical_calls :: CriticalFunctions -> [CStat] -> ([CStat], [Variable]) -- {{{1
normalize_critical_calls cf items =
  let (items', Ctx vars _ _) = runState (mapM tStmt items) (Ctx [] 0 [])
  in (concat items', vars)
  where
    tStmt :: CStat -> S [CStat]
    tStmt o@(CIf cond t e ni) =
      keepOrReplace [cond] o (\[cond'] -> CIf cond' t e ni) 

    tStmt o@(CReturn (Just expr) ni) =
      keepOrReplace [expr] o (\[expr'] -> CReturn (Just expr') ni)

    tStmt o@(CExpr (Just expr) ni)
      | isInNormalForm expr = return [o]
      | otherwise = keepOrReplace [expr] o (\[expr'] -> CExpr (Just expr') ni)

    tStmt o@(CGoto _ _) = return [o]

    tStmt x = $abort $ unexp x

    keepOrReplace :: [CExpr] -> CStat -> ([CExpr] -> CStat) -> S [CStat]
    keepOrReplace es s f = do
      (inits, es') <- liftM unzip $ mapM extractCriticalCalls es
      let allInits = concat inits
      if null allInits
        then return [s]
        else
          let s' = f es' in
          return $ allInits ++ [s']

    isInNormalForm (CCall _ _ _)                 = True
    isInNormalForm (CAssign _ _ (CCall _ _ _) _) = True
    isInNormalForm _                             = False

    extractCriticalCalls :: CExpr -> S ([CStat], CExpr)
    extractCriticalCalls expr = do
      modify (\(Ctx v c _) -> Ctx v c [])
      expr' <- everywhereM (mkM tExpr) expr
      (Ctx _ _ inits) <- get
      return (inits, expr')
      
    tExpr :: CExpr -> S CExpr
    tExpr call@(CCall (CVar callee _) _ _)
      | M.member (symbol callee) cf = do
          Ctx vars count inits <- get
          let
            decl       = newDecl callee count
            name       = symbol decl
            ni         = annotation call
            init       = CExpr (Just (CAssign CAssignOp (CVar (internalIdent name) undefNode) call ni)) ni
            var        = Variable decl name Nothing
          put $ Ctx (var : vars) (count + 1) (init : inits)
          return $ CVar (internalIdent name) undefNode

      | otherwise = return call
  
    tExpr o = return o

    newDecl :: Ident -> Int -> CDecl
    newDecl callee count =
      CDecl [CTypeSpec returnType] [(Just declarator, Nothing, Nothing)] undefNode
      where
        fd                = $fromJust_s $ M.lookup (symbol callee) cf
        (returnType, dds) = return_type_fd fd
        tmpVar            = internalIdent (varCrit count)
        declarator        = CDeclr (Just tmpVar) dds Nothing [] undefNode

data Ctx = Ctx {
    ctxVars  :: [Variable]
  , ctxCount :: Int
  , ctxInits :: [CStat]
  }

type S a = State Ctx a
