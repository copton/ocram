{-# LANGUAGE TemplateHaskell #-}
module Ocram.Intermediate.SequencializeBody
-- exports {{{1
(
  sequencialize_body
) where

-- imports {{{1
import Control.Monad (msum)
import Language.C.Syntax.AST
import Language.C.Data.Ident (internalIdent)
import Ocram.Symbols (symbol)
import Ocram.Util (abort, unexp)

sequencialize_body :: [CBlockItem] -> [CStat] -- {{{1
sequencialize_body = mergeLabels . removeDeadCode . flattenScopes

flattenScopes :: [CBlockItem] -> [CStat]  -- {{{2
-- |Inline all compound statements and unwrap all block items
flattenScopes = foldr flatten [] . map extract
  where
    extract (CBlockStmt s) = s
    extract x              = $abort $ unexp x

    flatten (CCompound _ items _) stmts = sequencialize_body items ++ stmts
    flatten (CIf cond t e ni)     stmts = CIf cond (unpack t) (fmap unpack e) ni : stmts
    flatten (CExpr Nothing _)     stmts = stmts
    flatten s                     stmts = s : stmts

    unpack (CCompound _ [CBlockStmt o@(CGoto _ _)] _) = o
    unpack o@(CGoto _ _)                              = o
    unpack x                                          = $abort $ unexp x

removeDeadCode :: [CStat] -> [CStat] -- {{{2
-- |Remove all statements between a return and the subsequent label
removeDeadCode = reverse . fst . foldr go ([], False) . reverse
  where
    go o@(CLabel _ _ _ _) (ss, True)  = (o : ss, False)
    go _                  (ss, True)  = (ss, True)
    go o@(CReturn _ _)    (ss, False) = (o : ss, True)
    go o                  (ss, False) = (o : ss, False)

mergeLabels :: [CStat] -> [CStat] -- {{{2
mergeLabels []    = []
mergeLabels stmts = case msum $ map scan (zip stmts (tail stmts)) of
  Nothing -> stmts
  Just (oldLabel, newLabel) -> mergeLabels (rewrite oldLabel newLabel stmts)
  where
    scan (CLabel first _ _ _, CLabel second _ _ _) = Just (symbol first, symbol second)
    scan _                                          = Nothing

    rewrite oldLabel newLabel = foldr go []
      where
        go o@(CLabel ident _ _ _) ss
          | symbol ident == oldLabel = ss
          | otherwise                = o : ss

        go (CIf cond t e ni) ss =
          CIf cond (rewriteGoto t) (fmap rewriteGoto e) ni : ss
          
        go o@(CGoto _ _ ) ss  = rewriteGoto o : ss
        go o@(CExpr _ _) ss   = o : ss
        go o@(CReturn _ _) ss = o : ss
        go x _                = $abort $ unexp x
    
        rewriteGoto o@(CGoto ident ni)
          | symbol ident == oldLabel = CGoto (internalIdent newLabel) ni
          | otherwise                = o
        rewriteGoto o                = $abort $ unexp o
