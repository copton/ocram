{-# LANGUAGE TemplateHaskell #-}
module Ocram.Intermediate.FlattenScopes
-- exports {{{1
(
  flatten_scopes
) where

-- imports {{{1
import Language.C.Syntax.AST
import Ocram.Util (abort, unexp)

flatten_scopes :: [CBlockItem] -> [CStat] -- {{{1
flatten_scopes = foldr flatten [] . map extract
  where
    extract (CBlockStmt s) = s
    extract x              = $abort $ unexp x

    flatten (CCompound _ items _) stmts = flatten_scopes items ++ stmts
    flatten s                     stmts = s : stmts
