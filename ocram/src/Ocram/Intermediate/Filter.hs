{-# LANGUAGE TemplateHaskell, GADTs #-}
module Ocram.Intermediate.Filter
-- exports {{{1
( 
    ir_constraints
  , ErrorCode(..) -- for testing
) where

-- imports {{{1
import Compiler.Hoopl (foldGraphNodes)
import Data.Generics (everything, mkQ)
import Language.C.Syntax.AST
import Language.C.Data.Node (nodeInfo)
import Ocram.Debug.Enriched (ENodeInfo, CExpr')
import Ocram.Text (OcramError, new_error)
import Ocram.Intermediate.Representation
import Ocram.Symbols (Symbol, symbol)
import Ocram.Util (abort, unexp, (?++))

import qualified Data.Set as S

data ErrorCode -- {{{1
  = PointerToCriticalFunction
  deriving (Eq, Enum, Show)

errorText :: ErrorCode -> String -- {{{2
errorText PointerToCriticalFunction =
  "Using pointer to critical function is not allowed."

ir_constraints :: S.Set Symbol -> Function -> Either [OcramError] () -- {{{1
ir_constraints = criticalFunctionPointer

criticalFunctionPointer :: S.Set Symbol -> Function -> Either [OcramError] () -- {{{2
criticalFunctionPointer sf fun = failOrPass $ foldGraphNodes check (fun_body fun) []
  where
    failOrPass [] = Right ()
    failOrPass es = Left es

    check :: Node e x -> [OcramError] -> [OcramError] -- {{{3
    check (Label _)            es    = es
    check (Cont _ nf)          es    = case nf of
      FirstNormalForm _ _ _         -> $abort "unexpected value"
      SecondNormalForm lhs _ _ _ _  -> scan lhs ++ es
    check (Stmt expr)          es    = scan expr ++ es
    check (Goto _)             es    = es 
    check (If cond _ _ _)      es    = scan cond ++ es 
    check (Call nf _)          es    = case nf of
      FirstNormalForm _ ps _        -> concatMap scan ps ++ es
      SecondNormalForm lhs _ _ ps _ -> scan lhs ++ concatMap scan ps ++ es
    check (Return Nothing _)     es  = es
    check (Return (Just expr) _) es  = scan expr ++ es

    scan :: CExpr'                 -> [OcramError] -- {{{3
    scan (CComma es _)              = concatMap scan es
    scan (CAssign _ lhs rhs _)      = scan lhs ++ scan rhs
    scan (CCond cond then_ else_ _) = scan cond ++ fmap scan then_ ?++ scan else_
    scan (CBinary _ lhs rhs _)      = scan lhs ++ scan rhs
    scan (CCast _ op _)             = scan op
    scan (CUnary _ op _)            = scan op
    scan (CSizeofExpr op _)         = scan op
    scan (CSizeofType _ _)          = []
    scan (CAlignofExpr op _)        = scan op
    scan (CAlignofType _ _)         = []
    scan (CComplexReal expr _)      = scan expr
    scan (CComplexImag expr _)      = scan expr
    scan (CIndex array index _)     = scan array ++ scan index
    scan (CCall (CVar _ _) ps _)    = concatMap scan ps
    scan (CCall callee ps _)        = scan callee ++ concatMap scan ps
    scan (CMember struct _ _ _)     = scan struct
    scan (CVar ident eni)
      | S.member (symbol ident) sf  = [newError PointerToCriticalFunction eni]
      | otherwise                   = []
    scan (CConst _)                 = []
    scan (CCompoundLit _ il _)      = everything (++) (mkQ [] scan) il
    scan o@(CStatExpr _ _)          = $abort $ unexp o
    scan (CLabAddrExpr _ _)         = []
    scan (CBuiltinExpr _)           = []

newError :: ErrorCode -> ENodeInfo -> OcramError -- {{{2
newError code eni = new_error (fromEnum code) (errorText code) (Just (nodeInfo eni))
