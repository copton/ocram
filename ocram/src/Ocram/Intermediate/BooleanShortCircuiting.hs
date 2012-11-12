{-# LANGUAGE TemplateHaskell #-}
module Ocram.Intermediate.BooleanShortCircuiting
-- export {{{1
(
  boolean_short_circuiting
) where

-- imports {{{1
import Control.Monad.State (State, runState, get, put, modify)
import Language.C.Syntax.AST
import Language.C.Data.Ident (internalIdent)
import Language.C.Data.Node (undefNode)
import Ocram.Intermediate.Representation (Variable(..))
import Ocram.Debug.Enriched (CExpr', CStat', CDesignator', CInit', aset, eun)
import Ocram.Symbols (symbol, Symbol)
import Ocram.Names (varBool)
import Ocram.Util (abort, unexp, unexp')

import qualified Data.Set as S

boolean_short_circuiting :: S.Set Symbol -> [CStat'] -> ([CStat'], [Variable]) -- {{{1
boolean_short_circuiting cf inputStmts =
  let
    (outputStmts, (Ctx _ vars)) = runState (mapM tStat inputStmts) (Ctx 0 [])
  in
    (concat outputStmts, vars)
  where
    tStat :: CStat' -> S [CStat'] -- {{{2
    tStat o@(CIf cond then_ else_ ni) =
      trav o cond (\e -> CIf e then_ else_ ni)
        
    tStat o@(CExpr (Just expr) ni) =
      trav o expr (\e -> CExpr (Just e) ni)

    tStat o@(CReturn (Just expr) ni) =
      trav o expr (\e -> CReturn (Just e) ni)

    tStat o@(CLabel _ _ _ _)    = return [o]
    tStat o@(CGoto _ _)         = return [o]
    tStat o@(CExpr Nothing _)   = return [o]
    tStat o@(CReturn Nothing _) = return [o]
    tStat o                     = $abort $ unexp o

    trav :: CStat' -> CExpr' -> (CExpr' -> CStat') -> S [CStat']
    trav stmt expr buildStmt = do
      subst <- traverse expr
      case subItems subst of
        [] -> return [stmt]
        items -> return $ map (aset (annotation expr)) $ items ++ [buildStmt (subExpr subst)]

    traverse :: CExpr' -> S (Substitution CExpr') -- {{{2
    traverse (CComma exprs ni) = do -- {{{3
      ss <- mapM traverse exprs
      let expr = CComma (map subExpr ss) ni
      let items = concatMap subItems ss
      let crit = or $ map subCritical ss
      return $ Substitution expr items crit

    traverse (CAssign op lhs rhs ni) = do -- {{{3
      lhs' <- traverse lhs
      rhs' <- traverse rhs
      let expr = CAssign op (subExpr lhs') (subExpr rhs') ni
      let items = subItems lhs' ++ subItems rhs'
      let crit = subCritical lhs' || subCritical rhs'
      return $ Substitution expr items crit

    traverse (CCond cond Nothing else_ ni) = do -- {{{3
      cond' <- traverse cond
      else_' <- traverse else_
      let expr = CCond (subExpr cond') Nothing (subExpr else_') ni
      let items = subItems cond' ++ subItems else_'
      let crit = subCritical cond' || subCritical else_'
      return $ Substitution expr items crit

    traverse (CCond cond (Just then_) else_ ni) = do -- {{{3
      cond' <- traverse cond
      then_' <- traverse then_
      else_' <- traverse else_ 
      let expr = CCond (subExpr cond') (Just (subExpr then_')) (subExpr else_') ni
      let items = subItems cond' ++ subItems then_' ++ subItems else_'
      let crit = subCritical cond' || subCritical then_' || subCritical else_'
      return $ Substitution expr items crit

    traverse (CBinary op lhs rhs ni) -- {{{3
      | isLogicalOp op = do
          lhs' <- traverse lhs
          rhs' <- traverse rhs
          if subCritical lhs' || subCritical rhs'
            then do
              idx <- next
              substitute idx op lhs' rhs'
            else
              let
                expr = CBinary op (subExpr lhs') (subExpr rhs') ni
                items = subItems lhs' ++ subItems rhs'
                crit = subCritical lhs' || subCritical rhs'
              in 
                return $ Substitution expr items crit
      | otherwise = do
          lhs' <- traverse lhs
          rhs' <- traverse rhs
          let expr = CBinary op (subExpr lhs') (subExpr rhs') ni
          let items = subItems lhs' ++ subItems rhs'
          let crit = subCritical lhs' || subCritical rhs'
          return $ Substitution expr items crit

    traverse (CCast decl expr ni) = do -- {{{3
      expr' <- traverse expr
      return $ Substitution (CCast decl (subExpr expr') ni) (subItems expr') (subCritical expr')

    traverse (CUnary op expr ni) = do -- {{{3
      expr' <- traverse expr
      return $ Substitution (CUnary op (subExpr expr') ni) (subItems expr') (subCritical expr')

    traverse (CSizeofExpr expr ni) = do -- {{{3
      expr' <- traverse expr
      return $ Substitution (CSizeofExpr (subExpr expr') ni) (subItems expr') (subCritical expr')
      
    traverse o@(CSizeofType _ _) = return $ Substitution o [] False -- {{{3

    traverse (CAlignofExpr expr ni) = do -- {{{3
      expr' <- traverse expr
      return $ Substitution (CAlignofExpr (subExpr expr') ni) (subItems expr') (subCritical expr')

    traverse (CComplexReal expr ni) = do -- {{{3
      expr' <- traverse expr
      return $ Substitution (CComplexReal (subExpr expr') ni) (subItems expr') (subCritical expr')

    traverse (CComplexImag expr ni) = do -- {{{3
      expr' <- traverse expr
      return $ Substitution (CComplexImag (subExpr expr') ni) (subItems expr') (subCritical expr')

    traverse (CIndex expr1 expr2 ni) = do -- {{{3
      expr1' <- traverse expr1
      expr2' <- traverse expr2
      let expr = CIndex (subExpr expr1') (subExpr expr2') ni
      let items = subItems expr1' ++ subItems expr2'
      let crit = subCritical expr1' || subCritical expr2'
      return $ Substitution expr items crit
      
    traverse (CCall v@(CVar iden _) params ni) = do -- {{{3
      ss <- mapM traverse params
      let expr = CCall v (map subExpr ss) ni
      let items = concatMap subItems ss
      let crit = if S.member (symbol iden) cf then True else or (map subCritical ss)
      return $ Substitution expr items crit

    traverse (CCall fun params ni) = do -- {{{3
      subFun <- traverse fun
      subPar <- mapM traverse params
      let expr = CCall (subExpr subFun) (map subExpr subPar) ni
      let items = (subItems subFun) ++ concatMap subItems subPar
      let crit = (subCritical subFun) || or (map subCritical subPar)
      return $ Substitution expr items crit

    traverse (CMember expr iden flag ni) = do -- {{{3
      expr' <- traverse expr
      return $ Substitution (CMember (subExpr expr') iden flag ni) (subItems expr') (subCritical expr')
      
    traverse o@(CVar _ _) = return $ Substitution o [] False -- {{{3

    traverse o@(CConst _ ) = return $ Substitution o [] False -- {{{3

    traverse (CCompoundLit decl initlst ni) = do -- {{{3
      initlst' <- mapM go initlst
      let expr = CCompoundLit decl (map subExpr initlst') ni
      let crit = or (map subCritical initlst')
      let items = concatMap subItems initlst'
      return $ Substitution expr items crit
      where
        go :: ([CDesignator'], CInit') -> S (Substitution ([CDesignator'], CInit'))
        go (partdes, initializer) = do
          partdes' <- mapM go' partdes
          initializer' <- go'' initializer
          let expr = (map subExpr partdes', subExpr initializer')
          let items = concatMap subItems partdes' ++ subItems initializer'
          let crit = subCritical initializer' || or (map subCritical partdes')
          return $ Substitution expr items crit

        go' :: CDesignator' -> S (Substitution CDesignator')
        go' (CArrDesig expr ni') = do
          expr' <- traverse expr
          return $ Substitution (CArrDesig (subExpr expr') ni') (subItems expr') (subCritical expr')
        go' o@(CMemberDesig _ _) = return $ Substitution o [] False
        go' o = $abort $ unexp o

        go'' :: CInit' -> S (Substitution CInit')
        go'' (CInitExpr expr ni') = do
          expr' <- traverse expr
          return $ Substitution (CInitExpr (subExpr expr') ni') (subItems expr') (subCritical expr')
        go'' (CInitList is ni') = do
          is' <- mapM go is
          let expr = CInitList (map subExpr is') ni'
          let items = concatMap subItems is'
          let crit = or (map subCritical is')
          return $ Substitution expr items crit

    traverse o = $abort $ unexp o -- {{{3
 
substitute :: Int -> CBinaryOp -> Substitution CExpr' -> Substitution CExpr' -> S (Substitution CExpr') -- {{{2
substitute idx op lhs rhs = do
  let
    un   = undefNode
    iden = internalIdent (varBool idx)
    ivar = EVariable $ CDecl [CTypeSpec (CBoolType un)] [(Just (CDeclr (Just iden) [] Nothing [] un) , Nothing, Nothing)] un 
    cvar = CVar iden eun
    [lhs_assign, rhs_assign] = map ((\x -> CExpr (Just x) (annotation x)) . (\x -> CAssign CAssignOp cvar ((neg . neg) x) (annotation x)) . subExpr) [lhs, rhs]
    cond = case op of
      CLorOp -> neg cvar
      CLndOp -> cvar
      o -> $abort $ unexp' o
  thenTarget <- next
  elseTarget <- next
  modify (\ctx -> ctx {ctxVars = ivar : ctxVars ctx})
  let
    if_ = CIf cond (mkGoto thenTarget) (Just (mkGoto elseTarget)) eun
    items = subItems lhs ++ (lhs_assign : if_ : mkLabel thenTarget : []) ++ subItems rhs ++ (rhs_assign : mkLabel elseTarget : [])
  return $ Substitution cvar items True
  where
    mkLabel i = CLabel (internalIdent (varBool i)) (CExpr Nothing eun) [] eun
    mkGoto  i = CGoto (internalIdent (varBool i)) eun

neg :: CExpr' -> CExpr' -- {{{2
neg expr = CUnary CNegOp expr (annotation expr)

isLogicalOp :: CBinaryOp -> Bool -- {{{2
isLogicalOp CLorOp = True
isLogicalOp CLndOp = True
isLogicalOp _ = False

next :: S Int -- {{{2
next = do
  Ctx count vars <- get
  put $ Ctx (count + 1) vars
  return count

data Ctx = Ctx { -- {{{2
    ctxCount :: Int
  , ctxVars  :: [Variable]
  }

type S = State Ctx -- {{{2

data Substitution a =  -- {{{2
  Substitution {
      subExpr :: a
    , subItems :: [CStat']
    , subCritical :: Bool
  }
