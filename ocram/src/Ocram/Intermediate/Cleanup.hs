{-# LANGUAGE TemplateHaskell #-}
module Ocram.Intermediate.Cleanup
-- exports {{{1
(
  cleanup
) where

-- imports {{{1
import Language.C.Syntax.AST
import Ocram.Util (abort, unexp)

cleanup :: [CBlockItem] -> [CStat] -- {{{1
cleanup = removeEmptyStatements . flattenScopes

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
