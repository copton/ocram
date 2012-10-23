module Ruab.Core.Internal where

-- imports {{{1
import Data.Generics (everywhereM, mkM)
import Data.List (find)
import Data.Maybe (isJust)
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
  Just vars -> do
    texpr <- parseExpression tstr
    let vars' = filter currentThread vars
    eexpr <- checkConversion (t2eExpr vars' texpr)
    return (printExpression eexpr)

  where

    currentThread (R.StaticVariable _, _) = True
    currentThread (R.AutomaticVariable tid' _, _) = tid == tid'

    checkConversion Nothing = Left "failed to convert expression"
    checkConversion (Just x) = Right x

parseExpression :: String -> Either String CExpr -- {{{2
parseExpression expr = case execParser_ expressionP (inputStreamFromString expr) nopos of
  Left e -> Left (show e)
  Right x -> Right x

t2eExpr :: [(R.Variable, R.FQN)] -> CExpr -> Maybe CExpr -- {{{2
t2eExpr vars = everywhereM (mkM trans)
  where
  trans :: CExpr -> Maybe CExpr
  trans (CVar ident _) = do
    (_, fqn) <- find ((==(identToString ident)) . R.varTName . fst) vars
    return $ CVar (internalIdent fqn) undefNode

  trans o = Just o

printExpression :: CExpr -> String -- {{{2
printExpression = show . pretty

findScope :: R.VarMap -> R.PRow -> Maybe R.RenameMap -- {{{2
findScope vm prow = fmap snd $ find inScope vm
  where
    inScope ((R.Scope start end), _) = prow >= start && prow <= end

in_critical_function :: R.DebugInfo -> R.PRow -> Bool -- {{{1
in_critical_function di = isJust . findScope (R.diVm di)
