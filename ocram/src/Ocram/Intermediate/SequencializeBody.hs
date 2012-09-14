{-# LANGUAGE TemplateHaskell #-}
module Ocram.Intermediate.SequencializeBody
-- exports {{{1
(
  sequencialize_body
) where

-- imports {{{1
import Language.C.Syntax.AST
import Ocram.Util (abort, unexp)

sequencialize_body :: [CBlockItem] -> [CStat] -- {{{1
sequencialize_body = foldr flatten [] . map extract
  where
    extract (CBlockStmt s) = s
    extract x              = $abort $ unexp x

    flatten (CCompound _ items _) stmts = sequencialize_body items ++ stmts
    flatten (CExpr Nothing _)     stmts = stmts
    flatten s                     stmts = s : stmts
