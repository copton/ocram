module Ruab.Core.Internal where

import Control.Monad (guard)
import Language.C.Parser (execParser_, expressionP)
import Language.C.Data.InputStream (inputStreamFromString)
import Language.C.Data.Position (initPos)
import Language.C.Syntax.AST (CExpr)
import Language.C.Pretty (pretty)

import qualified Ocram.Ruab as R

t2p_row' :: R.PreprocMap -> R.TRow -> Maybe R.PRow -- {{{1
t2p_row'(R.PreprocMap _ prows locs) trow = do
  guard (trow > 0)
  let (src, dst) = (last . takeWhile ((<=trow) . fst)) locs
  let prow = dst + (R.PRow . R.getTRow) (trow - src + 1)
  guard (prow <= prows)
  return prow

p2t_row' :: R.PreprocMap -> R.PRow -> Maybe R.TRow -- {{{1
p2t_row' ppm@(R.PreprocMap trows _ locs) prow = do
  guard (prow > 0)
  let (src, dst) = (last . takeWhile ((<=prow) . snd)) locs
  let trow = src + (R.TRow . R.getPRow) (prow - dst - 1)
  guard (trow <= trows)
  prow' <- t2p_row' ppm trow
  guard (prow' == prow)
  return trow

t2e_expr :: String -> Either String String  -- {{{1
t2e_expr str = do
  ast <- parseExpression str
  (return . printExpression . t2eExpr) ast

parseExpression :: String -> Either String CExpr -- {{{2
parseExpression expr = case execParser_ expressionP (inputStreamFromString expr) (initPos "<<user>>") of
  Left e -> Left (show e)
  Right x -> Right x

t2eExpr :: CExpr -> CExpr -- {{{2
t2eExpr = undefined

printExpression :: CExpr -> String -- {{{2
printExpression = show . pretty
