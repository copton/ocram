module Ruab.Core.Internal where

-- imports {{{1
import Data.Generics (everywhere, mkT)
import Data.List (find)
import Language.C.Data.Node (undefNode)
import Language.C.Data.Ident (identToString, internalIdent)
import Language.C.Data.InputStream (inputStreamFromString)
import Language.C.Data.Position (nopos)
import Language.C.Parser (execParser_, expressionP)
import Language.C.Pretty (pretty)
import Language.C.Syntax.AST (CExpr, CExpression(CVar))

import qualified Ocram.Ruab as R

t2e_expr :: R.VarMap -> R.ThreadId -> R.PRow -> String -> Either String String  -- {{{1
t2e_expr vm tid prow tstr = case findScope vm prow of
  Nothing   -> return tstr
  Just vars -> case parseExpression tstr of
    Left e -> Left e
    Right texpr -> 
      Right . printExpression . t2eExpr (filter currentThread vars) $ texpr

  where
    currentThread (R.StaticVariable _, _) = True
    currentThread (R.AutomaticVariable tid' _, _) = tid == tid'


parseExpression :: String -> Either String CExpr -- {{{2
parseExpression expr = case execParser_ expressionP (inputStreamFromString expr) nopos of
  Left e -> Left (show e)
  Right x -> Right x

t2eExpr :: [(R.Variable, R.FQN)] -> CExpr -> CExpr -- {{{2
t2eExpr vars = everywhere (mkT trans)
  where
  trans :: CExpr -> CExpr
  trans o@(CVar ident _) =
    case find ((==(identToString ident)) . R.varTName . fst) vars of
      Nothing -> o
      Just (_, fqn) -> CVar (internalIdent fqn) undefNode

  trans o = o

printExpression :: CExpr -> String -- {{{2
printExpression = show . pretty

findScope :: R.VarMap -> R.PRow -> Maybe R.RenameMap -- {{{2
findScope vm prow = fmap snd $ find inScope vm
  where
    inScope ((R.Scope start end), _) = prow >= start && prow <= end
