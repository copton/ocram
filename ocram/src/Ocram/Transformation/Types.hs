module Ocram.Transformation.Types where

import Ocram.Debug (ENodeInfo)
import Language.C.Syntax.AST

-- {{{1 Types
type CTranslUnit' = CTranslationUnit ENodeInfo
type CExpr' = CExpression ENodeInfo
type CBlockItem' = CCompoundBlockItem ENodeInfo
type CStat' = CStatement ENodeInfo
type CFunDef' = CFunctionDef ENodeInfo
type CDesignator' = CPartDesignator ENodeInfo
type CInit' = CInitializer ENodeInfo
type CDecl' = CDeclaration ENodeInfo
type CExtDecl' = CExternalDeclaration ENodeInfo
