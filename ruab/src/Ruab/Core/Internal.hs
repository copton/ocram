module Ruab.Core.Internal where

import Control.Monad (guard)
import Data.Generics (everywhereM, mkM)
import Langauge.C.Data.Node (undefNode)
import Language.C.Data.Ident (identToString, internalIdent)
import Language.C.Data.InputStream (inputStreamFromString)
import Language.C.Data.Position (nopos)
import Language.C.Parser (execParser_, expressionP)
import Language.C.Pretty (pretty)
import Language.C.Syntax.AST (CExpr, CExpression(CVar))

import qualified Ocram.Ruab as R
import qualified Data.Map as M

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

t2e_expr :: [String] -> R.VarMap -> R.ThreadId -> String -> String -> Either String String  -- {{{1
t2e_expr cf vm tid fname tstr
  | fname `elem` cf = do
      texpr <- parseExpression tstr
      eexpr <- maybe (Left "failed to convert expression") Right (t2eExpr vm tid fname texpr)
      (return . printExpression) eexpr
  | otherwise = return tstr

parseExpression :: String -> Either String CExpr -- {{{2
parseExpression expr = case execParser_ expressionP (inputStreamFromString expr) nopos of
  Left e -> Left (show e)
  Right x -> Right x

t2eExpr :: R.VarMap -> R.ThreadId -> String -> CExpr -> Maybe CExpr -- {{{2
t2eExpr vm tid fname = everywhereM (mkM trans)
  where
  trans :: CExpr -> Maybe CExpr
  trans (CVar ident _) = 
    let var = R.Variable tid fname (identToString ident) in
    fmap (buildCVar . R.getSubstitution) $ M.lookup var $ R.getVarMap vm

  trans o = Just o

  buildCVar symbol = CVar (internalIdent symbol) undefNode

printExpression :: CExpr -> String -- {{{2
printExpression = show . pretty
