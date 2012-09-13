{-# LANGUAGE TemplateHaskell #-}
module Ocram.Intermediate.Cleanup
-- exports {{{1
(
  cleanup
) where

-- imports {{{1
import Language.C.Syntax.AST
import Language.C.Data.Node (undefNode)
import Ocram.Util (abort, unexp)

cleanup :: [CBlockItem] -> [CStat] -- {{{1
cleanup = implicitReturn . removeEmptyStatements . flattenScopes

flattenScopes :: [CBlockItem] -> [CStat]
flattenScopes = foldr flatten [] . map extract
  where
    extract (CBlockStmt s) = s
    extract x              = $abort $ unexp x

    flatten (CCompound _ items _) stmts = flattenScopes items ++ stmts
    flatten s                     stmts = s : stmts

removeEmptyStatements :: [CStat] -> [CStat]
removeEmptyStatements = filter (not . emptyStmt)
  where
    emptyStmt (CExpr Nothing _) = True
    emptyStmt _                 = False

implicitReturn :: [CStat] -> [CStat]
implicitReturn items =
  let (lst:rest) = reverse items in
  case lst of 
    (CReturn _ _) -> items
    _             -> reverse $ (CReturn Nothing undefNode) : lst : rest
