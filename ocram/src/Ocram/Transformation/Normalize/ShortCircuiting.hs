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
        items -> return $ CCompound [] (items ++ [CBlockStmt (CIf (subExpr cond') then_ else_ ni)]) ni
    go o = return o

type Context = (Set.Set Symbol, Int)

data Substitution =  -- {{{2
  Substitution {
      subExpr :: CExpr'
    , subItems :: [CBlockItem']
    , subCritical :: Bool
  }

traverse :: CExpr' -> State Context Substitution -- {{{2
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

traverse (CComma exprs ni) = do
  ss <- mapM traverse exprs
  let expr = CComma (map subExpr ss) ni
  let items = concatMap subItems ss
  let crit = or $ map subCritical ss
  return $ Substitution expr items crit

traverse o = $abort $ unexp o


substitute :: Int -> CBinaryOp -> Substitution -> Substitution -> Substitution -- {{{2
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
    Substitution var items False

neg :: CExpr' -> CExpr' -- {{{2
neg exp = CUnary CNegOp exp (annotation exp)

isLogicalOp :: CBinaryOp -> Bool -- {{{2
isLogicalOp CLorOp = True
isLogicalOp CLndOp = True
isLogicalOp _ = False

next :: State Context Int -- {{{2
next = do
  (cf, idx) <- get
  put (cf, idx + 1)
  return idx
