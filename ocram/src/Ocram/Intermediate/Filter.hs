{-# LANGUAGE TemplateHaskell, GADTs #-}
module Ocram.Intermediate.Filter
-- exports {{{1
( 
  ir_constraints
) where

-- imports {{{1
import Compiler.Hoopl (foldGraphNodes)
import Data.Generics (everything, mkQ, extQ)
import Language.C.Syntax.AST
import Language.C.Data.Node (nodeInfo)
import Ocram.Debug.Enriched (ENodeInfo)
import Ocram.Text (OcramError, new_error)
import Ocram.Intermediate.Representation
import Ocram.Symbols (Symbol, symbol)
import Ocram.Util (abort, unexp)

import qualified Data.Set as S

data ErrorCode -- {{{1
  = PointerToCriticalFunction
  deriving (Eq, Enum)

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

    check :: Node e x -> [OcramError] -> [OcramError]
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

    check (Return Nothing _)     es   = es
    check (Return (Just expr) _) es   = scan expr ++ es

    scan var@(CVar _ _) = criticalPointer var
    scan expr = everything (++) (mkQ [] scanExpr `extQ` scanInit) expr

    scanExpr (CUnary CAdrOp var@(CVar _ _ ) _) = criticalPointer var
    scanExpr (CAssign _ _ var@(CVar _ _) _)    = criticalPointer var
    scanExpr _                                 = []

    scanInit (CInitExpr var@(CVar _ _) _)      = criticalPointer var
    scanInit _                                 = []

    criticalPointer (CVar ident eni)
      | S.member (symbol ident) sf
                  = [newError PointerToCriticalFunction eni]
      | otherwise = []
    criticalPointer e = $abort $ unexp e
      
newError :: ErrorCode -> ENodeInfo -> OcramError
newError code eni = new_error (fromEnum code) (errorText code) (Just (nodeInfo eni))
