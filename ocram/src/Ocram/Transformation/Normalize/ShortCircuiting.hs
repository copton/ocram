module Ocram.Transformation.Normalize.ShortCircuiting
(
  short_circuiting 
) where

-- imports {{{1
import Control.Monad.State (State, evalState, get, gets, put)
import Data.Generics (everywhereM, mkM)
import Language.C.Syntax.AST
import Ocram.Analysis (CriticalFunctions)
import Ocram.Debug
import Ocram.Symbols (symbol, Symbol)
import Ocram.Transformation.Util (ident)
import Ocram.Transformation.Names (tempBool)
import Ocram.Util (abort, unexp, unexp')

import qualified Data.Set as Set

short_circuiting :: CriticalFunctions -> CFunDef' -> CFunDef' -- {{{1
short_circuiting cf fd = evalState (everywhereM (mkM go) fd) (cf', 0)
  where
    cf' = Set.fromList cf
    go :: CStat' -> State Context CStat'
    go o@(CIf cond then_ else_ ni) = do
      cond' <- traverse cond
      case subItems cond' of
        [] -> return o
        items -> return $ block ni items $ CIf (subExpr cond') then_ else_ ni
    go o@(CExpr (Just expr) ni) = do
      expr' <- traverse expr
      case subItems expr' of
        [] -> return o
        items -> return $ block ni items $ CExpr (Just (subExpr expr')) ni
    go o@(CSwitch expr body ni) = do
      expr' <- traverse expr
      case subItems expr' of
        [] -> return o
        items -> return $ block ni items $ CSwitch (subExpr expr') body ni
    go o@(CReturn (Just expr) ni) = do
      expr' <- traverse expr
      case subItems expr' of
        [] -> return o
        items -> return $ block ni items $ CReturn (Just (subExpr expr')) ni
    go o = return o 

    block ni items stat = CCompound [] (items ++ [CBlockStmt stat]) ni

type Context = (Set.Set Symbol, Int)

data Substitution a =  -- {{{2
  Substitution {
      subExpr :: a
    , subItems :: [CBlockItem']
    , subCritical :: Bool
  }

traverse :: CExpr' -> State Context (Substitution CExpr') -- {{{2
traverse (CComma exprs ni) = do
  ss <- mapM traverse exprs
  let expr = CComma (map subExpr ss) ni
  let items = concatMap subItems ss
  let crit = or $ map subCritical ss
  return $ Substitution expr items crit

traverse (CAssign op lhs rhs ni) = do
  lhs' <- traverse lhs
  rhs' <- traverse rhs
  let expr = CAssign op (subExpr lhs') (subExpr rhs') ni
  let items = subItems lhs' ++ subItems rhs'
  let crit = subCritical lhs' || subCritical rhs'
  return $ Substitution expr items crit

-- traverse CCond TODO

traverse (CBinary op lhs rhs ni)
  | isLogicalOp op = do
      lhs' <- traverse lhs
      rhs' <- traverse rhs
      if subCritical lhs' || subCritical rhs'
        then do
          idx <- next
          return $ substitute idx op lhs' rhs'
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

traverse (CCast decl expr ni) = do
  expr' <- traverse expr
  return $ Substitution (CCast decl (subExpr expr') ni) (subItems expr') (subCritical expr')

traverse (CUnary op expr ni) = do
  expr' <- traverse expr
  return $ Substitution (CUnary op (subExpr expr') ni) (subItems expr') (subCritical expr')

traverse (CSizeofExpr expr ni) = do
  expr' <- traverse expr
  return $ Substitution (CSizeofExpr (subExpr expr') ni) (subItems expr') (subCritical expr')
  
traverse o@(CSizeofType _ _) = return $ Substitution o [] False

traverse (CAlignofExpr expr ni) = do
  expr' <- traverse expr
  return $ Substitution (CAlignofExpr (subExpr expr') ni) (subItems expr') (subCritical expr')

traverse (CComplexReal expr ni) = do
  expr' <- traverse expr
  return $ Substitution (CComplexReal (subExpr expr') ni) (subItems expr') (subCritical expr')

traverse (CComplexImag expr ni) = do
  expr' <- traverse expr
  return $ Substitution (CComplexImag (subExpr expr') ni) (subItems expr') (subCritical expr')

traverse (CIndex expr1 expr2 ni) = do
  expr1' <- traverse expr1
  expr2' <- traverse expr2
  let expr = CIndex (subExpr expr1') (subExpr expr2') ni
  let items = subItems expr1' ++ subItems expr2'
  let crit = subCritical expr1' || subCritical expr2'
  return $ Substitution expr items crit
  
traverse (CCall v@(CVar iden _) params ni) = do -- {{{2
  ss <- mapM traverse params
  cf <- gets fst
  let expr = CCall v (map subExpr ss) ni
  let items = concatMap subItems ss
  let crit = if Set.member (symbol iden) cf then True else or (map subCritical ss)
  return $ Substitution expr items crit

traverse (CCall fun params ni) = do
  subFun <- traverse fun
  subPar <- mapM traverse params
  let expr = CCall (subExpr subFun) (map subExpr subPar) ni
  let items = (subItems subFun) ++ concatMap subItems subPar
  let crit = (subCritical subFun) || or (map subCritical subPar)
  return $ Substitution expr items crit

traverse (CMember expr iden flag ni) = do
  expr' <- traverse expr
  return $ Substitution (CMember (subExpr expr') iden flag ni) (subItems expr') (subCritical expr')
  
traverse o@(CVar _ _) = return $ Substitution o [] False

traverse o@(CConst _ ) = return $ Substitution o [] False

traverse o@(CCompoundLit decl initlst ni) = do
  initlst' <- mapM go initlst
  let expr = CCompoundLit decl (map subExpr initlst') ni
  let crit = or (map subCritical initlst')
  let items = concatMap subItems initlst'
  return $ Substitution expr items crit
  where
    go :: ([CDesignator'], CInit') -> State Context (Substitution ([CDesignator'], CInit'))
    go (partdes, initializer) = do
      partdes' <- mapM go' partdes
      initializer' <- go'' initializer
      let expr = (map subExpr partdes', subExpr initializer')
      let items = concatMap subItems partdes' ++ subItems initializer'
      let crit = subCritical initializer' || or (map subCritical partdes')
      return $ Substitution expr items crit

    go' :: CDesignator' -> State Context (Substitution CDesignator')
    go' (CArrDesig expr ni) = do
      expr' <- traverse expr
      return $ Substitution (CArrDesig (subExpr expr') ni) (subItems expr') (subCritical expr')
    go' o@(CMemberDesig _ _) = return $ Substitution o [] False
    go' o = $abort $ unexp o

    go'' :: CInit' -> State Context (Substitution CInit')
    go'' (CInitExpr expr ni) = do
      expr' <- traverse expr
      return $ Substitution (CInitExpr (subExpr expr') ni) (subItems expr') (subCritical expr')
    go'' (CInitList initlst ni) = do
      initlst' <- mapM go initlst
      let expr = CInitList (map subExpr initlst') ni
      let items = concatMap subItems initlst'
      let crit = or (map subCritical initlst')
      return $ Substitution expr items crit
 
substitute :: Int -> CBinaryOp -> Substitution CExpr' -> Substitution CExpr' -> Substitution CExpr' -- {{{2
substitute idx op lhs rhs =
  let
    iden = ident (tempBool idx)
    decl = CBlockDecl $ CDecl [CTypeSpec (CIntType un)] [(Just (CDeclr (Just iden) [] Nothing [] un) , Nothing, Nothing)] un
    var = CVar iden un
    [lhs_assign, rhs_assign] = map (CBlockStmt . (\x -> CExpr (Just x) un) . (\x -> CAssign CAssignOp var ((neg . neg) x) (annotation x)) . subExpr) [lhs, rhs]
    cond = case op of
      CLorOp -> neg var
      CLndOp -> var
      o -> $abort $ unexp' o
    if_ = CBlockStmt $ CIf cond (CCompound [] (subItems rhs ++ [rhs_assign]) un) Nothing un
    items = subItems lhs ++ [decl, lhs_assign, if_]
  in
    Substitution var items True

neg :: CExpr' -> CExpr' -- {{{2
neg expr = CUnary CNegOp expr (annotation expr)

isLogicalOp :: CBinaryOp -> Bool -- {{{2
isLogicalOp CLorOp = True
isLogicalOp CLndOp = True
isLogicalOp _ = False

next :: State Context Int -- {{{2
next = do
  (cf, idx) <- get
  put (cf, idx + 1)
  return idx
